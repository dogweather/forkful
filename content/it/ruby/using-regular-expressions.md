---
title:                "Ruby: Utilizzo delle espressioni regolari"
programming_language: "Ruby"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/ruby/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Perché

Se sei un programmatore Ruby, probabilmente hai sentito parlare di espressioni regolari. Ma perché dovresti usarle? Le espressioni regolari sono un modo efficiente per manipolare e cercare stringhe di testo all'interno di un programma. Sono potenti strumenti utili per risparmiare tempo e fatica nella ricerca e sostituzione di testo.

## Come

Per utilizzare le espressioni regolari in Ruby, è necessario utilizzare la classe `Regexp` e i suoi metodi. Ecco un semplice esempio di come utilizzare un'espressione regolare per cercare una parola all'interno di una stringa:

```Ruby
stringa = "Ciao a tutti!"
parola_da_cercare = /Ciao/
risultato = stringa.match(parola_da_cercare)
puts risultato  #=> Ciao
```

Nell'esempio sopra, abbiamo creato una variabile `stringa` contenente la frase "Ciao a tutti!" e una variabile `parola_da_cercare` che indica la parola che vogliamo cercare. Utilizzando il metodo `match`, la variabile `risultato` conterrà la corrispondenza della nostra espressione regolare all'interno della stringa. Possiamo quindi stampare il risultato a schermo.

Oltre alla ricerca, è possibile utilizzare le espressioni regolari per scomporre una stringa, sostituire parti di essa e valutare se una stringa soddisfa un determinato modello. Ci sono numerosi metodi della classe `Regexp` che consentono di eseguire queste operazioni in modo efficiente.

## Deep Dive

Oltre ai metodi di base, le espressioni regolari in Ruby offrono diverse opzioni avanzate per personalizzare le ricerche. Ad esempio, è possibile utilizzare `i` per eseguire una ricerca case-insensitive o `m` per considerare le nuove righe nella ricerca. È anche possibile utilizzare gruppi e quantificatori per creare espressioni regolari più complesse.

Inoltre, Ruby offre il supporto alle espressioni regolari POSIX, che sono utilizzate per eseguire ricerche più precise nei testi.

Per saperne di più su come utilizzare le espressioni regolari in Ruby, vale la pena fare un'immersione più profonda nella documentazione ufficiale e nei tutorial online.

## Vedi anche

- Tutorial sulle espressioni regolari: https://www.rubyguides.com/2015/06/ruby-regex/;
- Documentazione ufficiale di Ruby sulle espressioni regolari: https://ruby-doc.org/core-2.7.2/Regexp.html.