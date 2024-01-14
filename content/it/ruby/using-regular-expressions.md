---
title:                "Ruby: Uso delle espressioni regolari"
simple_title:         "Uso delle espressioni regolari"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/ruby/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Perché

Se sei un programmatore di Ruby probabilmente hai sentito parlare delle espressioni regolari, ma potresti chiederti perché dovresti imparare a usarle. Le espressioni regolari sono una potente strumento per manipolare stringhe di testo, e ti permettono di cercare e sostituire parti di una stringa in modo flessibile. Una volta padroneggiate, le espressioni regolari saranno un'aggiunta utile al tuo kit di strumenti di programmazione.

## Come

Per iniziare ad utilizzare le espressioni regolari in Ruby, devi prima creare un oggetto di tipo Regexp utilizzando l'operatore `/`. Ad esempio:

```
pattern = /Ruby/
```

Questo creerà un oggetto che può essere utilizzato per cercare la parola "Ruby" all'interno di una stringa. Per trovare una corrispondenza, puoi utilizzare i metodi `match` o `=~`. Ad esempio:

```
"Mi piace programmare in Ruby".match(pattern)
```

Questo restituirà un oggetto MatchData contenente informazioni sulla posizione e lunghezza della corrispondenza trovata.

Per sostituire una parte di una stringa con un'altra, puoi utilizzare il metodo `gsub` e passare come argomento l'espressione regolare e la stringa di sostituzione. Ad esempio:

```
"Il mio linguaggio preferito è Ruby".gsub(pattern, "Python")
```

Questo sostituirà ogni occorrenza della parola "Ruby" con "Python".

## Profondità

Le espressioni regolari possono sembrare complicate all'inizio, ma una volta padroneggiati i concetti fondamentali, diventano una potente risorsa per manipolare stringhe di testo. Una delle funzionalità più utili delle espressioni regolari è la possibilità di utilizzare quantificatori per cercare un numero variabile di corrispondenze. Ad esempio, utilizzando `+` dopo un carattere specifico, troverai tutte le occorrenze di quel carattere che si ripetono una o più volte all'interno della stringa.

Un'altro vantaggio delle espressioni regolari è la possibilità di utilizzare i cosiddetti "gruppi di cattura", ossia porzioni di stringa tra parentesi che possono essere poi utilizzati per manipolare la stringa di output. Ad esempio, cercando `/(\w+) (\w+)/` puoi catturare il primo e secondo nome separatamente e poi invertirli con `/\2 \1/`.

Se desideri approfondire ulteriormente le espressioni regolari in Ruby, puoi consultare la documentazione ufficiale del linguaggio  o seguire alcuni tutorial online.

## Vedi anche

- [Documentazione ufficiale di Ruby: Espressioni regolari](https://ruby-doc.org/core-3.0.0/Regexp.html)
- [Tutorial di Codecademy su espressioni regolari in Ruby](https://www.codecademy.com/learn/learn-ruby/modules/learn-ruby-introduction-to-regex-u/cheatsheet)
- [Guida completa alle espressioni regolari in Ruby](https://www.rubyguides.com/2015/06/ruby-regex/)