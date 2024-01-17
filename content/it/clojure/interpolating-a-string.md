---
title:                "Interpolazione di una stringa"
html_title:           "Clojure: Interpolazione di una stringa"
simple_title:         "Interpolazione di una stringa"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/clojure/interpolating-a-string.md"
---

{{< edit_this_page >}}

## Cosa e Perché?
Interpolare una stringa significa inserire dei valori dinamici all'interno di una stringa. I programmatori lo fanno per rendere più flessibile e leggibile il codice, in particolare quando è necessario manipolare o combinare variabili e stringhe.

## Come Fare:
```Clojure
(def nome "Giorgio")

(println "Ciao, mi chiamo " nome "e sono un amante della programmazione.")
```
Output:
```Ciao, mi chiamo Giorgio e sono un amante della programmazione.```

## Approfondimento:
L'interpolazione di stringhe è una tecnica molto comune nel mondo della programmazione, soprattutto tra i linguaggi di scripting come Clojure. Essa consente di combinare la logica di programmazione con la visualizzazione dei dati. Un'alternativa all'interpolazione di stringhe è l'utilizzo di una libreria di formattazione, come la libreria Java String.format(). Nella sua implementazione, Clojure utilizza la funzione str interpolazione per sostituire i valori all'interno di una stringa.

## Vedi Anche:
* [Clojure Doc - String Interpolation](https://clojure.org/guides/string_interpolation)
* [Java String.format()](https://docs.oracle.com/javase/8/docs/api/java/lang/String.html#format(java.lang.String,java.lang.Object...))