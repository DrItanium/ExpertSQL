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
; Clause.clp - Represents a clause in a SQL statement 
; Written by Joshua Scoggins (10/12/2012)
;------------------------------------------------------------------------------
(defclass Clause (is-a HasAParent)
 (slot Previous (type SYMBOL INSTANCE-NAME INSTANCE-ADDRESS) 
  (allowed-classes Clause Query))
 (slot Next (type SYMBOL INSTANCE-NAME INSTANCE-ADDRESS)
  (allowed-classes Clause Query))
 (slot Action (type SYMBOL))
 (multislot Value))


(defclass Function (is-a HasAParent)
 (slot FunctionName (type SYMBOL INSTANCE-NAME INSTANCE-ADDRESS))
 (multislot Value (allowed-classes Clause Query)))

;(defgeneric make-clause "Creates a new clause")
(defmethod make-clause ((?Action SYMBOL) ?Value)
 (make-instance of Clause (Action ?Action) (Value ?Value)))
(defmethod make-clause ((?Action SYMBOL) ?Value
                        (?Next SYMBOL INSTANCE-NAME INSTANCE-ADDRESS) 
                        (?Previous SYMBOL INSTANCE-NAME INSTANCE-ADDRESS))
 (make-instance of Clause (Action ?Action) (Value ?Value) (Next ?Next)
  (Previous ?Previous)))

(defmethod make-clause ((?Action SYMBOL) ?Value (?Next SYMBOL INSTANCE-NAME
                                                 INSTANCE-ADDRESS))
 (make-instance of Clause (Action ?Action) (Value ?Value) (Next ?Next)
  (Previous nil)))

(defmethod make-function ((?Name SYMBOL) $?Value)
 (make-instance of Function (FunctionName ?Name) (Value $?Value)))

(defmethod SELECT (?Values (?Next SYMBOL INSTANCE-NAME INSTANCE-ADDRESS))
 (make-clause SELECT ?Values ?Next))

(defmethod SELECT (?Values)
 (make-clause SELECT ?Values))

(defmethod FROM (?Values (?Next SYMBOL INSTANCE-NAME INSTANCE-ADDRESS))
 (make-clause FROM ?Values ?Next))

(defmethod FROM (?Values) 
 (make-clause FROM ?Values))

(defmethod WHERE (?Values (?Next SYMBOL INSTANCE-NAME INSTANCE-ADDRESS))
 (make-clause WHERE ?Values ?Next))

(defmethod WHERE (?Values)
 (make-clause WHERE ?Values ))

(defmethod AVG (?Body)
 (make-function AVG ?Body))
