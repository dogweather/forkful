---
title:                "Capitalizzare una stringa"
html_title:           "Ruby: Capitalizzare una stringa"
simple_title:         "Capitalizzare una stringa"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/ruby/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Perché

Capitalizzare una stringa può essere utile in diverse situazioni, ad esempio nel processo di formattazione di dati per la visualizzazione in un'applicazione o nella creazione di URL amichevoli per i motori di ricerca.

## Come fare

Per capitalizzare una stringa in Ruby, possiamo utilizzare il metodo `capitalize` come di seguito:

```Ruby
stringa = "ciao mondo"
puts stringa.capitalize
```
Output: "Ciao mondo"

Inoltre, esiste anche il metodo `capitalize!` che modifica direttamente la stringa originale anziché restituire una nuova stringa. Dato che Ruby è un linguaggio orientato agli oggetti, dobbiamo utilizzare il punto esclamativo (`!`) per indicare che il metodo modificherà la variabile originale.

```Ruby
stringa = "ciao mondo"
stringa.capitalize!
puts stringa
```
Output: "Ciao mondo"

Possiamo anche capitalizzare solo la prima lettera di ogni parola in una stringa utilizzando il metodo `titleize`. Ad esempio:

```Ruby
stringa = "ciao mondo"
puts stringa.titleize
```
Output: "Ciao Mondo"

## Approfondimento

Oltre ai metodi `capitalize` e `capitalize!`, esiste anche il metodo `upcase` che trasforma tutti i caratteri in maiuscolo. Ciò può essere utile quando vogliamo semplicemente rendere una stringa tutta maiuscola, senza dover preoccuparci delle lettere maiuscole o minuscole originarie.

```Ruby
stringa = "ciao mondo"
puts stringa.upcase
```
Output: "CIAO MONDO"

Inoltre, Ruby ci offre anche il metodo `swapcase` che scambia i caratteri maiuscoli con quelli minuscoli e viceversa. Ad esempio:

```Ruby
stringa = "Ciao Mondo"
puts stringa.swapcase
```
Output: "cIAO mONDO"

Oltre ai metodi specifici per la capitalizzazione, possiamo anche utilizzare il metodo generale `gsub` per sostituire una stringa con una versione capitalizzata di se stessa. Ad esempio:

```Ruby
stringa = "ciao mondo"
puts stringa.gsub(/\b\w/) { |lettera| lettera.upcase }
```
Output: "Ciao Mondo"

## Vedi anche

- [Metodi di stringa Ruby (in inglese)](https://ruby-doc.org/core-2.7.0/String.html)
- [Tutorial di Ruby (in italiano)](https://it.wikibooks.org/wiki/Programmare_in_Ruby)