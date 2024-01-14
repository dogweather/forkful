---
title:    "Ruby: Utilizzando le espressioni regolari"
keywords: ["Ruby"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/ruby/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Perché

Se hai mai programmato in Ruby, probabilmente ti sei imbattuto in termini come "espressioni regolari" o "regex". Ma cosa sono esattamente le espressioni regolari e perché dovresti imparare ad usarle? Le espressioni regolari sono un potente strumento per la manipolazione di stringhe di testo, aiutandoti a trovare e sostituire pattern di caratteri. Se stai cercando di fare operazioni su testo in modo efficiente, le espressioni regolari sono un must nella tua cassetta degli attrezzi da sviluppatore Ruby.

## Come Utilizzarle

Per utilizzare le espressioni regolari in Ruby, devi prima creare un oggetto Regex attraverso il metodo `Regexp.new()` o utilizzando la sintassi abbreviata `/pattern/`. Ad esempio, per trovare tutte le vocali in una stringa, puoi utilizzare il seguente codice:

```ruby
str = "Ciao amici!"
vowels = /[aeiou]/
puts str.scan(vowels)
```

Questo produrrà l'output `[i, i, o, a, i]`, poiché queste sono le vocali presenti nella stringa. Puoi anche utilizzare le espressioni regolari per sostituire parte di una stringa con un'altra. Ad esempio, se vuoi sostituire tutte le vocali in una stringa con l'asterisco "*", puoi utilizzare il seguente codice:

```ruby
str = "Ciao amici!"
vowels = /[aeiou]/
puts str.gsub(vowels, "*")
```

Questo produrrà l'output `"Ca** *m*c*!"`.

## Approfondimento

Oltre alle operazioni di base come trovare e sostituire pattern, le espressioni regolari in Ruby offrono una varietà di opzioni e metodi avanzati. Ad esempio, puoi utilizzare il modificatore `/i` per effettuare una ricerca senza distinzione tra maiuscole e minuscole. Inoltre, puoi utilizzare il metodo `.match()` per trovare la prima corrispondenza di una regex all'interno di una stringa. Esplora le diverse opzioni e metodi disponibili nelle espressioni regolari in Ruby per migliorare le tue abilità di programmazione.

## Vedi Anche

- [Documentazione Ruby sulle espressioni regolari](https://ruby-doc.org/core-2.7.2/RegExp.html)
- [Tutorial sulle espressioni regolari in Ruby](https://www.tutorialspoint.com/ruby/ruby_regular_expressions.htm)
- [Esercizi pratici per imparare le espressioni regolari in Ruby](https://www.rubyguides.com/2015/06/ruby-regex/)