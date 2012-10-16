;Copyright (c) 2012, Joshua Scoggins 
;All rights reserved.
;
;Redistribution and use in source and binary forms, with or without
;modification, are permitted provided that the following conditions are met:
;    * Redistributions of source code must retain the above copyright
;      notice, this list of conditions and the following disclaimer.
;    * Redistributions in binary form must reproduce the above copyright
;      notice, this list of conditions and the following disclaimer in the
;      documentation and/or other materials provided with the distribution.
;    * Neither the name of Joshua Scoggins nor the
;      names of its contributors may be used to endorse or promote products
;      derived from this software without specific prior written permission.
;
;THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
;ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
;DISCLAIMED. IN NO EVENT SHALL Joshua Scoggins BE LIABLE FOR ANY
;DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
;(INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
;LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
;ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
;------------------------------------------------------------------------------
; CREATE.clp - Represents the CREATE SQL clause as rules
; Written by Joshua Scoggins (10/12/2012)
;------------------------------------------------------------------------------
;TODO: Add corresponding objects
(defclass Column (is-a USER)
 (slot Name (type SYMBOL))
 (slot Type (type SYMBOL))
 (multislot Definitions))
(defclass VarCharColumn (is-a Column)
 (slot Length (type NUMBER)))
(defclass Table (is-a USER)
 (slot Name (type SYMBOL))
 (slot Temporary (type SYMBOL) (allowed-values FALSE TRUE))
 (multislot Columns (type INSTANCE) (allowed-classes Column))
 (slot RowClassName (type SYMBOL)))

(defmessage-handler Table add-column (?column)
 (slot-direct-insert$ Columns 1 ?column))
(defclass TableRow (is-a USER))

(defrule create-new-permanent-table
         ?f <- (CREATE TABLE ?name { $?contents })
         (not (exists (object (is-a Table) (name ?name))))
         =>
         (retract ?f)
         (bind ?className (make-instance ?name of Table (Name ?name)))
         (assert (For ?className build $?contents)))

(defrule create-new-temporary-table
         ?f <- (CREATE TEMPORARY TABLE ?name { $?contents })
         (not (exists (object (is-a Table) (name ?name))))
         =>
         (retract ?f)
         (bind ?className (make-instance ?name of Table (Temporary TRUE)))
         (assert (For ?className build $?contents)))

;TODO: Add rules handling the creation of tables, etc that already exist
(defrule shatter-column-imbue 
         ?f <- (For ?className build ?name ?type $?args , $?rest)
         ?table <- (object (is-a Table) (name ?className))
         ;we need some way to keep track of type information as well as actual data
         =>
         (send ?table add-column
          (make-instance of Column 
           (Name ?name) (Type ?type)
           (Definitions $?args)))
         (retract ?f)
         (assert (For ?className build $?rest)))

(defrule shatter-column-imbue-varchar
         ?f <- (For ?className build ?name VARCHAR { ?size } $?args , $?rest)
         ?table <- (object (is-a Table) (name ?className))
         ;we need some way to keep track of type information as well as actual data
         =>
         (send ?table add-column
          (make-instance of VarCharColumn 
           (Name ?name) 
           (Length ?size)
           (Definitions $?args)))
         (retract ?f)
         (assert (For ?className build $?rest)))

(defrule retract-column-imbue-shatter
         ?f <- (For ?className build)
         =>
         (retract ?f)
         (assert (For ?className make row type)))


(defrule make-row-type-for-table
         ?f <- (For ?className make row type)
         (test (not (class-existp (sym-cat entry-row- (instance-name-to-symbol
                                                 ?className)))))
         ?table <- (object (is-a Table) (name ?className) (Columns $?columns))
         =>
         (bind ?rowClassName (sym-cat entry-row- (instance-name-to-symbol
                                               ?className)))
         (modify-instance ?table (RowClassName ?rowClassName))
         (make-instance of ClassBuilder (class-name ?rowClassName) 
                                        (isa TableRow))
         (retract ?f)
         (assert (For ?rowClassName transform columns $?columns)))
          
                           ;(Columns $columns)))

(defrule build-column-for-type
 ?f <- (For ?rowClassName transform columns ?column $?rest)
 ?objects <- (object (is-a ClassBuilder) (class-name ?rowClassName))
 (object (is-a Column) (name ?column) (Name ?name) (Type ?type) 
  (Definitions $?definitions))
 =>
 (retract ?f)
 (assert (For ?rowClassName transform columns $?rest))
 (slot-insert$ ?objects slots 1
  (make-instance of ClassSlot 
   (slot-name ?name)
   (type (switch ?type
          (case VARCHAR then STRING)
          (case NUMBER then NUMBER)
          (case FLOAT then FLOAT)
          (default VARIABLE))))))

(defrule build-column-for-type-retract
 ?f <- (For ?n transform columns)
 =>
 (assert (Create type ?n))
 (retract ?f))

(defrule build-row-type
 ?f <- (Create type ?n)
 ?b <- (object (is-a ClassBuilder) (class-name ?n))
 =>
 (retract ?f)
 (send ?b build)
 (unmake-instance ?b))

;TODO: Convert each column to a corresponding slot for the target type


