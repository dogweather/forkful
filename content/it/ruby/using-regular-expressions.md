---
title:                "Utilizzare le espressioni regolari"
html_title:           "Ruby: Utilizzare le espressioni regolari"
simple_title:         "Utilizzare le espressioni regolari"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/ruby/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Che cos'è e perché?

Le espressioni regolari, o "regular expressions" in inglese, sono uno strumento potentissimo per manipolare e cercare testo all'interno di stringhe. Sono utilizzate principalmente dai programmatori per automatizzare determinate operazioni e trovare corrispondenze all'interno di grandi quantità di testo.

## Come si usa:

Usare le espressioni regolari in Ruby è molto semplice. Basta utilizzare il metodo `=~` su una stringa e passare come argomento l'espressione regolare desiderata, racchiusa tra due slash `//`. Vediamo un esempio:

```Ruby
text = "Questo è un testo di prova"
puts text =~ /testo/  # output: 15
```

In questo caso, il metodo `=~` restituisce l'indice in cui viene trovata la prima occorrenza della stringa "testo" all'interno della variabile `text`, ovvero 15. È possibile utilizzare anche una sintassi più compatta, inserendo direttamente l'espressione regolare dopo il segno `=~`:

```Ruby
puts /testo/ =~ text  # output: 15
```

Se si vuole sostituire un match con una stringa diversa, si può utilizzare il metodo `gsub` sulla stringa e passare come primo argomento l'espressione regolare e come secondo argomento la stringa sostituita. Vediamo un esempio:

```Ruby
puts text.gsub(/testo/, "esempio")  # output: Questo è un esempio di prova
```

## Approfondimento:

Le espressioni regolari sono state sviluppate negli anni '50 da matematici e informatici per codificare pattern di ricerca complessi. Oggi, molte lingue di programmazione, tra cui Ruby, supportano l'utilizzo delle espressioni regolari.

Invece di utilizzare le espressioni regolari, i programmatori possono ricorrere ad altri metodi per manipolare il testo, come ad esempio l'utilizzo delle funzioni `include?` e `index` in Ruby. Tuttavia, le espressioni regolari risultano più potenti e flessibili per la maggior parte delle operazioni di ricerca e sostituzione di testo.

Per chi volesse approfondire l'argomento, esistono molti tutorial e documentazione online riguardanti le espressioni regolari in Ruby, sia in italiano che in inglese.

## Vedi anche:

- Documentazione ufficiale di Ruby sulle espressioni regolari: https://ruby-doc.org/core/Regexp.html
- Tutorial sulle espressioni regolari in Ruby: https://www.rubyguides.com/ruby-tutorial/regexp/
- Articolo su Espressioni Regolari e Ruby: https://www.rubyguides.com/ruby-tutorial/regexp/
- `intro-exp-ruby` gem: https://github.com/julik/intro-exp-ruby