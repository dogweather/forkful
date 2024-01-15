---
title:                "Ricerca e sostituzione di testo"
html_title:           "Ruby: Ricerca e sostituzione di testo"
simple_title:         "Ricerca e sostituzione di testo"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/ruby/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Perché
Ci sono molte ragioni per cui potresti voler cercare e sostituire del testo all'interno del tuo codice Ruby. Potresti voler aggiornare vecchie variabili, standardizzare la formattazione o semplicemente correggere errori di battitura.

## Come fare
Per cercare e sostituire del testo in Ruby, puoi utilizzare il metodo `gsub` (abbreviazione di "global substitution") con una combinazione di espressioni regolari e stringhe. Ecco un esempio:

```Ruby
stringa = "Benvenuto al mio blog"
nuova_stringa = stringa.gsub("blog", "sito web")
puts nuova_stringa
```
Output: "Benvenuto al mio sito web"

Puoi anche utilizzare espressioni regolari per cercare e sostituire più elementi contemporaneamente. Ad esempio, se volessi sostituire tutte le vocali minuscole in una stringa con la lettera "x", puoi usare l'espressione regolare `/[aeiou]/`:

```Ruby
stringa = "Ciao, come stai?"
nuova_stringa = stringa.gsub(/[aeiou]/, "x")
puts nuova_stringa
```
Output: "Cxx,xmx sxtx?"

## Approfondimento
Il metodo `gsub` è molto utile quando si lavora con stringhe ma è importante tenere presente che è case sensitive, quindi fa distinzione tra maiuscole e minuscole. Se vuoi sostituire del testo ignorando le maiuscole e le minuscole, puoi utilizzare il metodo `gsub!`, che ha un'opzione per specificare che la ricerca deve essere case insensitive:

```Ruby
stringa = "Benvenuto al mio sito web"
nuova_stringa = stringa.gsub!(/B/, "C")
puts nuova_stringa
```
Output: "Cenvenuto al mio sito web"

È inoltre possibile utilizzare il metodo `gsub` su una collezione come un array, per sostituire del testo in tutti gli elementi all'interno di quell'array:

```Ruby
membri = ["Anna, Bob, Charlie"]
membri.gsub(/[aeiou]/, "x")
puts membri
```
Output: ["Annx, Bxb, Chxrlxx"]

## Vedi anche
- [Metodo gsub su Ruby Docs](https://ruby-doc.org/core-2.7.0/String.html#method-i-gsub)
- [Tutorial sulle espressioni regolari in Ruby](https://www.rubyguides.com/2015/06/ruby-regex/)
- [Video introduttivo su come utilizzare espressioni regolari in Ruby](https://www.youtube.com/watch?v=VrPdJ2YyHL8&t=230s)