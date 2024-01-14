---
title:                "Arduino: Utilizzare le espressioni regolari"
programming_language: "Arduino"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/arduino/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Perché Usare le Espressioni Regolari con Arduino

Le espressioni regolari sono uno strumento potentissimo per la manipolazione dei dati in qualsiasi linguaggio di programmazione, compreso Arduino. Sono particolarmente utili per la ricerca e la sostituzione di pattern di testo, facendo risparmiare tempo e fatica nel processo di sviluppo di un progetto. In questa guida, impareremo come utilizzare le espressioni regolari con Arduino per semplificare il nostro codice.

## Come Utilizzare le Espressioni Regolari con Arduino

Per utilizzare le espressioni regolari con Arduino, dobbiamo prima importare la libreria “Regex” all’interno del nostro sketch. Per farlo, basta cliccare su Sketch > Includi Libreria > Aggiungi Libreria e cercare “Regex” nella barra di ricerca.

Una volta importata la libreria, possiamo iniziare a utilizzare le espressioni regolari nel nostro codice. Ad esempio, se vogliamo controllare se una variabile di tipo String contiene una stringa specifica, possiamo utilizzare il metodo “match” della libreria Regex. Ecco un esempio di codice:

```Arduino
#include <Regex.h>

String testo = "Benvenuti su questo blog post!";
if (Regex.match("blog post", testo)) {
  Serial.println("La variabile contiene la frase cercata.");
} else {
  Serial.println("La variabile non contiene la frase cercata.");
}
```

Questa è solo una delle molte funzionalità che possiamo sfruttare utilizzando le espressioni regolari, ma ci permette di avere un’idea di come possono semplificare il nostro codice.

## Approfondimento sulle Espressioni Regolari

Le espressioni regolari seguono un formato specifico per indicare un pattern di testo, e possono includere caratteri speciali e quantificatori per rendere la ricerca ancora più precisa. Una volta imparato il loro utilizzo, possiamo utilizzarle per controllare input da sensori, analizzare dati o anche per effettuare controlli su wireless shields.

Una delle cose più utili delle espressioni regolari è la loro capacità di effettuare sostituzioni di testo, ovvero la capacità di trovare e rimpiazzare una stringa con un’altra. Ad esempio, se volessimo cambiare la parola “blog” con “sito” in tutto il nostro testo, possiamo utilizzare il metodo “replace” della libreria Regex. Ecco un esempio di codice:

```Arduino
#include <Regex.h>

String testo = "Benvenuti su questo blog post!";
testo.replace("blog", "sito");
Serial.println(testo);
```

Questo codice dovrebbe stampare “Benvenuti su questo sito post!” sulla seriale.

## Vedi anche

- [Documentazione ufficiale di Regex per Arduino](https://github.com/nickgammon/ArduinoRegexp)
- [Esempi di utilizzo delle espressioni regolari in Arduino](https://learn.sparkfun.com/tutorials/regular-expressions-in-arduino/all)
- [Guida dettagliata alle espressioni regolari in generale](https://www.regular-expressions.info/)