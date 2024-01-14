---
title:    "Gleam: Ricerca e sostituzione di testo"
keywords: ["Gleam"]
---

{{< edit_this_page >}}

## Perché

C'è un compito fastidioso che tutti i programmatori devono affrontare: sostituire pezzi di testo all'interno del codice. È una di quelle piccole cose che possono richiedere molto tempo se fatte manualmente. Ma non temere, Gleam è qui per aiutarti! In questo post, imparerai come utilizzare Gleam per cercare e sostituire il testo all'interno del tuo codice in modo efficiente e veloce.

## Come

Per prima cosa, devi importare il modulo `gleam/text` nella tua applicazione Gleam. Questo modulo contiene alcune funzioni utili per il manipolamento del testo, inclusa la funzione `replace`.

```Gleam
// Importa il modulo gleam/text
import gleam/text

// Esegui la sostituzione del testo
let new_string = text.replace("Ciao mondo!", "mondo", "Italia")
```

Questa funzione prende tre argomenti: la stringa originale, il testo da cercare e il testo con cui sostituirlo. Nel nostro esempio, stiamo sostituendo "mondo" con "Italia" all'interno della stringa "Ciao mondo!". Se eseguiamo questo codice, il valore di `new_string` sarà "Ciao Italia!".

Puoi anche passare una funzione come argomento da utilizzare per raccogliere il testo di sostituzione. Ad esempio:

```Gleam
// Definisci una funzione di sostituzione
fn to_lowercase(string) { String.to_lower(string) }

// Esegui la sostituzione con la funzione
let new_string = text.replace("HELLO WORLD!", "WORLD", to_lowercase)
```

In questo caso, la funzione `replace` userà la nostra funzione `to_lowercase` per trasformare il testo di sostituzione in lettere minuscole prima di eseguire la sostituzione. Il valore di `new_string` sarà "HELLO world!".

## Deep Dive

Ora che hai una comprensione di base di come usare la funzione `replace`, puoi approfondire le possibilità di ricerca e sostituzione di text in Gleam. Il modulo `gleam/text` contiene anche altre funzioni utili come `replace_all`, che esegue la sostituzione di tutte le occorrenze di un testo all'interno di una stringa, e `replace_regexp`, che utilizza espressioni regolari per trovare e sostituire il testo.

Inoltre, Gleam ha una libreria esterna chiamata `gleam/regexp`, che offre una vasta gamma di funzioni per lavorare con espressioni regolari. Puoi utilizzare queste funzioni insieme alla funzione `replace_regexp` per avere ancora più controllo sulla ricerca e sostituzione del testo.

## Vedi anche

- Documentazione ufficiale della funzione `replace` in Gleam: https://gleam.run/modules/gleam/text#replace
- Tutorial di Gleam sulle espressioni regolari: https://gleam.run/tutorials/regular-expressions
- Documentazione ufficiale della libreria `gleam/regexp`: https://gleam.run/modules/gleam/regexp