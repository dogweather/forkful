---
title:    "Gleam: Utilizzando le espressioni regolari"
keywords: ["Gleam"]
---

{{< edit_this_page >}}

#Perché
Le espressioni regolari (o regex) sono uno strumento fondamentale per i programmatori per effettuare ricerche e manipolazioni di testo avanzate. Sono utili in molte situazioni, come il parsing di dati, la validazione di input utente e la sostituzione di testo in un documento.

#Come Usarle
Per utilizzare le espressioni regolari in Gleam, è necessario importare il modulo `gleam/regex` e utilizzare la funzione `regex.match` per cercare una corrispondenza all'interno di una stringa. Ecco un esempio di come cercare un numero di telefono in un testo utilizzando una regex:

```Gleam
import gleam/regex

let testo = "Il mio numero di telefono è 555-123-4567."
let regex = regex.regex("\\d{3}-\\d{3}-\\d{4}")
let risultato = regex.match(testo)

debug(risultato)  // Output: Some("\\d{3}-\\d{3}-\\d{4}")
```

Come si può notare, la funzione `regex.regex` accetta una stringa contenente la regex desiderata. Utilizzando `\\d` si indica una cifra e `{n}` indica quante volte deve essere ripetuta quella parte della regex. Nel nostro esempio, `{3}` viene usato per indicare che si vogliono 3 cifre consecutive. Per maggiori informazioni sulle regex, si consiglia di consultare la sezione di "Deep Dive".

#Deep Dive
Le espressioni regolari sono composte da una serie di simboli e caratteri che aiutano a creare pattern di testo da cercare. Alcuni esempi di simboli comunemente usati sono `*` per indicare qualsiasi carattere, `+` per indicare uno o più ripetizioni di un carattere e `[]` per indicare un set di caratteri da cercare.

Inoltre, è possibile utilizzare gli operatori `|` per indicare alternative all'interno della regex e `()` per raggruppare parti della regex. Ad esempio, la regex `\\d{3}-?\\d{3}-\\d{4}` corrisponderà sia a numeri di telefono con o senza il trattino dopo i primi 3 numeri.

Utilizzare le espressioni regolari può richiedere un po' di pratica e sperimentazione per ottenere il pattern desiderato, ma una volta padroneggiate sono uno strumento molto potente per manipolare testo.

#Vedi Anche
- Documentazione ufficiale Gleam sul modulo `gleam/regex`: https://gleam.run/modules/regex
- Guida completa alle espressioni regolari: https://www.regular-expressions.info/
- Tool online per testare ed esplorare le regex: https://regex101.com/