---
title:                "Utilizzare le espressioni regolari"
html_title:           "Clojure: Utilizzare le espressioni regolari"
simple_title:         "Utilizzare le espressioni regolari"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/clojure/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Perché Utilizzare le Espressioni Regolari?

Se ti trovi spesso a cercare e manipolare testi in modo efficiente, le espressioni regolari possono essere un'arma potente nel tuo arsenale di strumenti di programmazione. Con il loro aiuto, puoi facilmente cercare, sostituire e manipolare testi in modo più semplice e veloce rispetto alle tradizionali operazioni di stringhe.

## Come Utilizzare le Espressioni Regolari in Clojure

Per utilizzare le espressioni regolari in Clojure, puoi utilizzare la funzione `re-find` o `re-matches`. Ad esempio:

```Clojure
(re-find #"\d+" "Questo è un testo 123")
; Output: "123"

(re-matches #"^\w+" "Questa è una parola")
; Output: ["Questa"]
```

Le espressioni regolari sono sempre racchiuse tra `#""` mentre il testo su cui applicare l'espressione è racchiuso tra ` "" `.

Puoi anche utilizzare le espressioni regolari per sostituire parti di un testo con la funzione `re-sub`. Ad esempio:

```Clojure
(re-sub #"otto" "otto e dieci" "nove")
; Output: "nove e dieci"
```

Inoltre, puoi utilizzare diverse opzioni all'interno delle espressioni regolari, come ad esempio l'uso di `?i` per ignorare le maiuscole e minuscole o `?x` per permettere di aggiungere spazi e commenti all'interno dell'espressione.

## Approfondimento sull'utilizzo delle Espressioni Regolari

Le espressioni regolari sono basate su un linguaggio formale chiamato "Regular Expression Language" (o Regex) che permette di creare pattern di ricerca e sostituzione in modo più flessibile ed efficiente rispetto alle tradizionali operazioni di stringhe. Per imparare di più sul loro utilizzo, puoi consultare la documentazione ufficiale di Clojure dedicata alle espressioni regolari o seguire alcuni tutorial online.

## Vedi Anche

- [Documentazione Ufficiale di Clojure su Espressioni Regolari](https://clojuredocs.org/clojure.core/re-find)
- [Tutorial su Espressioni Regolari in Clojure](https://eugenp.com/2018/07/23/clojure-regular-expressions-tutorial/)