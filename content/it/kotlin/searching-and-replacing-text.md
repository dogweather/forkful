---
title:                "Cercare e sostituire testo"
html_title:           "Kotlin: Cercare e sostituire testo"
simple_title:         "Cercare e sostituire testo"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/kotlin/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Perché

Molti programmatori si trovano spesso a dover effettuare ricerca e sostituzione di testo nei loro codici. Questo è necessario per correggere errori di battitura, aggiornare nomi di variabili o implementare funzionalità speciali. Utilizzare Kotlin per eseguire l'operazione di ricerca e sostituzione rende il processo più semplice e veloce.

## Come fare

Per eseguire la ricerca e sostituzione del testo in Kotlin, è necessario utilizzare il metodo `replace` su un oggetto String. Ecco un esempio di codice che sostituisce la parola "ciao" con "salve" in una stringa:

```
val str = "Ciao mondo"
val nuova = str.replace("ciao", "salve")
println(nuova)

// Output: Salve mondo
```

Utilizzando questo metodo, è possibile sostituire in modo rapido e semplice tutte le occorrenze di una parola o un'espressione in una stringa. Inoltre, è possibile specificare opzioni aggiuntive per modificare il comportamento del metodo, come l'ignorare maiuscole/minuscole o utilizzare un'espressione regolare.

## Approfondimento

Il metodo `replace` accetta due parametri: quello che si sta cercando di sostituire e quello con cui lo si vuole sostituire. Entrambi i parametri possono essere stringhe o espressioni regolari. Quindi, è possibile utilizzare questa funzione anche per eseguire sostituzioni più complesse, come sostituire solo le vocali di una parola con delle stelle.

Un'altra opzione utile è il metodo `replaceFirst`, che sostituisce solo la prima occorrenza trovata nella stringa. Ciò può essere utile se si vuole sostituire solo una parte specifica della stringa.

## Vedi anche

- Documentazione ufficiale di Kotlin sul metodo `replace`: https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/replace.html
- Tutorial su come utilizzare espressioni regolari in Kotlin: https://kotlinlang.org/docs/regular-expressions.html