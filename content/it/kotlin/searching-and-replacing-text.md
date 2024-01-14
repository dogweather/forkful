---
title:                "Kotlin: Ricerca e sostituzione di testo"
simple_title:         "Ricerca e sostituzione di testo"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/kotlin/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

##Perché

La ricerca e la sostituzione di testo sono una parte fondamentale della programmazione in Kotlin. Questa funzionalità ti permette di modificare rapidamente e in modo efficace il testo all'interno del tuo codice, risparmiandoti tempo e sforzi. Continua a leggere per scoprire come utilizzare questa funzionalità essenziale!

##Come farlo

Codice Kotlin inline:
```
val testo = "Questo è un esempio di testo da modificare"

testo.replace("esempio", "campione")

// Output: Questo è un campione di testo da modificare
```

Codice Kotlin multilinea:
```
val testo = """Questo è un esempio di
              testo da modificare"""

testo.replace("esempio", "campione")

// Output: Questo è un campione di
// testo da modificare
```

In entrambi gli esempi, abbiamo utilizzato il metodo `replace` per cercare e sostituire il testo desiderato. Si può anche specificare un valore di indice in modo da indicare quale istanza del testo dovrebbe essere sostituita, ad esempio `testo.replace("esempio", "campione", startIndex = 1)`. Questo cambierà solo la seconda occorrenza della parola "esempio" nel testo.

##Approfondimento

Ci sono anche altre funzionalità interessanti legate alla ricerca e replace del testo in Kotlin che potresti voler esplorare. Il metodo `replaceFirst` ti permette di sostituire solo la prima occorrenza della parola, mentre `replaceBefore` e `replaceAfter` ti consentono di specificare una stringa di delimitazione in modo da sostituire solo il testo prima o dopo di essa. Puoi anche utilizzare l'operatore di assegnazione `=` per sostituire una stringa di testo direttamente, ad esempio `testo = testo.replace("esempio", "campione")`. Sperimenta con queste opzioni e scopri cosa funziona meglio per te!

##Vedi anche

- [Documentazione ufficiale di Kotlin sulla ricerca e replace di testo](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/replace.html)
- [Guida dettagliata su come utilizzare replace in Kotlin](https://blog.kotlin-academy.com/string-replace-in-kotlin-f08b4430a021)
- [Tutorial video su come utilizzare il metodo replace in Kotlin](https://www.youtube.com/watch?v=_YvQHCVteEE)