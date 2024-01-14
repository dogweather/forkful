---
title:                "Ruby: Cercare e sostituire testo"
programming_language: "Ruby"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/ruby/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Perché

La ricerca e la sostituzione di testo sono fondamentali per la programmazione in Ruby. Sia che si stia cercando di correggere errori di battitura o di aggiornare dati in file, la funzione di ricerca e sostituzione semplifica notevolmente il processo di editing del codice.

## Come Fare

Per effettuare una ricerca e sostituzione di testo in Ruby, è possibile utilizzare il metodo .gsub(). Questo metodo accetta due parametri, il testo da cercare e il testo con cui sostituirlo. Ad esempio:

```Ruby
"My name is John".gsub("John", "Jane")
```

Output:

```Ruby
"My name is Jane"
```

È possibile anche utilizzare espressioni regolari per rendere la ricerca e la sostituzione più flessibili. Ad esempio, per sostituire tutte le vocali in una stringa con una "x", si può utilizzare il seguente codice:

```Ruby
"My name is John".gsub(/[aeiou]/, "x")
```

Output:

```Ruby
"Mx nxmx xs Jxhn"
```

## Approfondimento

La funzione di ricerca e sostituzione in Ruby è possibile grazie al metodo .gsub(), che sta per "global substitution". Per fare ricerche e sostituzioni più complesse, si può utilizzare anche il metodo .gsub!(), che sostituisce direttamente il testo all'interno della stringa originale.

Inoltre, è possibile utilizzare opzioni aggiuntive nel metodo .gsub() per rendere la ricerca e la sostituzione case-sensitive o per sostituire solo un numero limitato di occorrenze.

## Vedi Anche

- [Documentazione Ruby: String#gsub](https://ruby-doc.org/core-2.7.1/String.html#method-i-gsub)
- [Tutorial su espressioni regolari in Ruby](https://www.rubyguides.com/2015/06/ruby-regex/)
- [Esempi su come utilizzare il metodo .gsub()](https://www.rubyguides.com/2014/09/ruby-string-methods/#gsub)

Se vuoi approfondire ulteriormente le funzionalità di ricerca e sostituzione di testo in Ruby, ti consigliamo di dare un'occhiata a questi link utili. Buona codifica!