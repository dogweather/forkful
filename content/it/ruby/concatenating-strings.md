---
title:                "Concatenazione di stringhe"
html_title:           "Bash: Concatenazione di stringhe"
simple_title:         "Concatenazione di stringhe"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/ruby/concatenating-strings.md"
---

{{< edit_this_page >}}

## Che Cosa & Perché?
La concatenazione delle stringhe in Ruby è l'operazione di unione di due o più stringhe per formarne una sola. È una pratica comune utilizzata dai programmi per creare messaggi dinamici, formattare dati e molto altro.

## Come fare:
Concatenare le stringhe in Ruby è molto semplice. Ecco due esempi principali:

```Ruby
# Usare l'operatore '+'
stringa1 = "Ciao, "
stringa2 = "mondo!"
risultato = stringa1 + stringa2
puts risultato
# Output: "Ciao, mondo!"
```

```Ruby
# Usare il metodo 'concat' 
stringa1 = "Ciao, "
stringa2 = "mondo!"
stringa1.concat(stringa2)
puts stringa1
# Output: "Ciao, mondo!"
```

## Approfondimento
La concatenazione delle stringhe esiste da quando sono stati inventati i linguaggi di programmazione. In Ruby, si può anche usare l'operatore '<<' o il metodo '<<' per concatenare stringhe, ma questi metodi modificano la stringa originale a differenza dell'operatore '+'.

Un'altra alternativa è l'utilizzo del metodo 'join':

```Ruby
stringhe = ["Ciao", "mondo"]
risultato = stringhe.join(", ")
puts risultato
# Output: "Ciao, mondo"
```

L'unione di stringhe è una operazione comune e quasi tutti i linguaggi di programmazione includono funzionalità per farlo. In Ruby, la concatenazione di stringhe crea sempre un nuovo oggetto stringa, consumando memoria.

## Vedi Anche
- [Documentazione Ruby su String](https://ruby-doc.org/core-2.7.0/String.html)
- [Guida alla Concatenazione di Stringhe su RubyGuides](https://www.rubyguides.com/2018/08/ruby-string-concatenation/).
- [Stackoverflow: Differenze tra '+', '<<' e 'concat'](https://stackoverflow.com/questions/4684446/why-is-used-for-string-concatenation-in-ruby)