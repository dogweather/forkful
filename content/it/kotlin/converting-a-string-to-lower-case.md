---
title:                "Convertire una stringa in minuscolo"
html_title:           "Kotlin: Convertire una stringa in minuscolo"
simple_title:         "Convertire una stringa in minuscolo"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/kotlin/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Perché

Ci sono molte ragioni per cui potresti voler convertire una stringa in minuscolo in Kotlin. Ad esempio, potresti aver ricevuto una stringa da una fonte esterna e desideri uniformare il formato prima di utilizzarla nel tuo codice.

## Come Fare

La conversione di una stringa in minuscolo in Kotlin è molto semplice grazie alla funzione `toLowerCase()` disponibile sulla classe String. Vediamo un esempio pratico:

```Kotlin
val stringa = "KOTLIN IN MINUSCOLO"
val stringaMinuscola = stringa.toLowerCase()
println(stringaMinuscola)
```

Questo codice stamperà a schermo la stringa "kotlin in minuscolo", come desiderato. Se invece desideri convertire una stringa direttamente durante la sua creazione, puoi farlo utilizzando il modificatore `lowercase` come segue:

```Kotlin
val stringaMinuscola = "kotlin in minuscolo".lowercase()
```

Entrambe le opzioni ti permettono di ottenere una stringa in minuscolo a partire da una stringa esistente.

## Approfondimento

La funzione `toLowerCase()` è in realtà una delle molte funzioni disponibili per la manipolazione delle stringhe in Kotlin. Alcune altre funzioni utili includono:

- `toUpperCase()` per convertire una stringa in maiuscolo
- `capitalize()` per rendere maiuscola solo la prima lettera di una stringa
- `reversed()` per invertire l'ordine dei caratteri nella stringa

Oltre a queste funzioni, puoi anche utilizzare le espressioni regolari per manipolare le stringhe in modi più complessi. Ad esempio, puoi utilizzare l'espressione regolare `"[A-Z]"` per convertire tutte le lettere maiuscole in una stringa in minuscolo.

## Vedi Anche

- Documentazione ufficiale di Kotlin sulle stringhe (https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/-string/)
- Guida a Kotlin per principianti (https://developer.android.com/kotlin/first)
- Articolo su come utilizzare gli espressioni regolari in Kotlin (https://www.baeldung.com/kotlin-regular-expressions)