---
title:                "Ruby: Maiuscolare una stringa"
programming_language: "Ruby"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/ruby/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Perché

Capitalizzare una stringa è un'operazione comune nella programmazione Ruby e può avere diverse finalità, come rendere il testo più leggibile o manipolare i dati in modo più preciso. In questo articolo, ti mostrerò come capitalizzare una stringa in modo efficace utilizzando il linguaggio Ruby.

## Come

Per capitalizzare una stringa in Ruby, puoi utilizzare il metodo `capitalize` sulla stringa stessa. Questo metodo cambierà il primo carattere della stringa in maiuscolo e lascerà gli altri caratteri in minuscolo.

```
my_string = "ciao a tutti"
puts my_string.capitalize
# Output: Ciao a tutti
```

In alternativa, puoi utilizzare il metodo `upcase` per rendere tutti i caratteri della stringa in maiuscolo.

```
my_string = "ciao a tutti"
puts my_string.upcase
# Output: CIAO A TUTTI
```

Se vuoi capitalizzare solo la prima lettera di ogni parola nella stringa, puoi utilizzare il metodo `capitalize!` combinato con il metodo `split` per suddividere la stringa in un array di parole.

```
my_string = "ciao a tutti"
my_string.split.each { |word| word.capitalize! }.join(' ')
# Output: Ciao A Tutti
```

## Deep Dive

Quando si utilizza il metodo `capitalize` su una stringa, è importante notare che non solo il primo carattere diventerà maiuscolo, ma anche ogni altro carattere alla fine di una frase o di una parola. Pertanto, se hai una stringa con più di una frase, il risultato potrebbe essere diverso da quanto ci si aspetta.

Un altro modo per capitalizzare una stringa in più parole è utilizzando il gem `titleize`. Questo gem esegue una capitalizzazione più intelligente delle parole in una stringa tenendo conto delle regole grammaticali della lingua inglese.

```
my_string = "ciao a tutti. come state?"
puts my_string.titleize
# Output: Ciao A Tutti. Come State?
```

## Vedi anche

- [Documentazione di Ruby sulla manipolazione di stringhe](https://ruby-doc.org/core-2.7.2/String.html)
- [Guida di Ruby su come capitalizzare una stringa](https://www.rubyguides.com/2015/06/ruby-string-capitalize-method/)
- [Gem titleize per una capitalizzazione più precisa delle stringhe](https://rubygems.org/gems/titleize)