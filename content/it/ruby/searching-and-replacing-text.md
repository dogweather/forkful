---
title:    "Ruby: Cercare e sostituire il testo"
keywords: ["Ruby"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/ruby/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Perché
La ricerca e la sostituzione del testo sono due operazioni comuni che possono essere utilizzate per fare modifiche rapide e precise a un file di testo. Nella programmazione, questo può essere particolarmente utile quando si lavora con grandi quantità di codice o quando si desidera apportare modifiche uniformi a righe di testo simili.

## Come Fare
Per eseguire una ricerca e sostituzione di testo in Ruby, è possibile utilizzare il metodo `gsub` sulle stringhe. Questo metodo accetta due argomenti: il testo da cercare e il testo da sostituire. Ad esempio, se si desidera sostituire tutte le occorrenze della parola "cane" con la parola "gatto" in una stringa, è possibile utilizzare il seguente codice:

```Ruby
stringa = "Il mio cane è il mio migliore amico."
stringa.gsub!("cane", "gatto")

puts stringa
```
Output: "Il mio gatto è il mio migliore amico."

Facciamo un altro esempio utilizzando un blocco nei metodi `gsub`. Utilizzando `%r{}` per includere espressioni regolari all'interno della nostra ricerca, possiamo sostituire l'anno in una stringa con una stringa vuota:

```Ruby
stringa = "Amo il codice! È il 2019."
stringa.gsub!(%r{ \d{4} }, "")

puts stringa
```
Output: "Amo il codice! È il ."

## Approfondimento
Oltre al semplice utilizzo del metodo `gsub`, Ruby offre anche altre utili funzioni per la ricerca e la sostituzione del testo, come `scan` e `match`. Inoltre, è possibile utilizzare espressioni regolari per una maggiore flessibilità nei criteri di ricerca e sostituzione del testo.

Ad esempio, se si desidera cercare una parola che inizia con una lettera maiuscola in una stringa, è possibile utilizzare l'espressione regolare `/\b[A-Z][a-zA-Z]*\b/`. Questo corrisponderà a qualsiasi parola che inizia con una lettera maiuscola. 

Per maggiori informazioni su espressioni regolari e sulle funzioni Ruby per la ricerca e la sostituzione del testo, è possibile consultare la documentazione ufficiale di Ruby o seguire i link nella sezione "Vedi Anche" di questo articolo.

## Vedi Anche
- [Documentazione Ruby - Metodo gsub](https://ruby-doc.org/core-2.6.3/String.html#method-i-gsub)
- [Documentazione Ruby - Modulo String](https://ruby-doc.org/core-2.6.3/String.html)
- [Tutorial espressioni regolari Ruby](https://www.tutorialspoint.com/ruby/ruby_regular_expressions.htm)