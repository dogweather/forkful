---
title:                "Utilizzo delle espressioni regolari"
html_title:           "Ruby: Utilizzo delle espressioni regolari"
simple_title:         "Utilizzo delle espressioni regolari"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/ruby/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Perché

Se sei un programmatore Ruby, probabilmente hai sentito parlare di espressioni regolari (o "regex"). Questi potenti strumenti di manipolazione dei dati sono ampiamente utilizzati per la ricerca e la sostituzione di testo all'interno di stringhe. Se vuoi risparmiare tempo e fatica nella manipolazione dei dati, le espressioni regolari sono un'ottima opzione.

## Come Fare

Per utilizzare le espressioni regolari in Ruby, è prima necessario definire un oggetto "Regexp" che contiene l'espressione che si desidera confrontare. Ad esempio, per cercare una parola all'interno di una stringa, possiamo utilizzare il seguente codice:

```Ruby
stringa = "Questa è una frase per provare le espressioni regolari."
parola = Regexp.new("provare")
risultato = parola.match(stringa)
puts risultato[0] #stamperà "provare"
```

Come puoi vedere, il codice utilizza la sintassi "Regexp.new" per definire l'espressione regolare e poi la applica alla stringa utilizzando il metodo "match". Il risultato viene quindi restituito come un oggetto "MatchData", che può essere utilizzato per ottenere una varietà di informazioni sulle corrispondenze trovate.

Puoi anche utilizzare le espressioni regolari per sostituire parti di una stringa con del nuovo testo utilizzando il metodo "sub". Ad esempio:

```Ruby
stringa = "Questo capitolo si concentra sulle espressioni regolari."
nuova_stringa = stringa.sub(Regexp.new("espressioni regolari"), "regex")
puts nuova_stringa #stamperà "Questo capitolo si concentra sulle regex."
```

C'è molto di più da imparare sulle espressioni regolari in Ruby, ma queste sono solo alcune basi per iniziare.

## Approfondimenti

Se vuoi approfondire ulteriormente le tue conoscenze sulle espressioni regolari in Ruby, ecco alcuni consigli:

- [La guida ufficiale](https://ruby-doc.org/core-2.7.2/Regexp.html) della documentazione Ruby per le espressioni regolari.
- [Un tutorial](https://www.rubyguides.com/2015/06/ruby-regex/) dettagliato su come utilizzare le espressioni regolari in Ruby.
- [Il playground](https://rubular.com/) online di espressioni regolari per aiutarti a testare e sperimentare con le tue espressioni.

## Vedi Anche

- [Documentazione ufficiale di Ruby](https://www.ruby-lang.org/it/documentation/)
- [Guida introduttiva a Ruby](https://www.codecademy.com/learn/learn-ruby) su Codecademy.