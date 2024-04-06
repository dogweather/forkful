---
date: 2024-01-20 17:57:18.227559-07:00
description: "Come fare: La ricerca e sostituzione di testo si basa su concetti degli\
  \ anni '50 e '60, quando programmatori utilizzavano editor di testo per manipolare\u2026"
lastmod: '2024-04-05T22:50:57.471871-06:00'
model: gpt-4-1106-preview
summary: La ricerca e sostituzione di testo si basa su concetti degli anni '50 e '60,
  quando programmatori utilizzavano editor di testo per manipolare codice.
title: Ricerca e sostituzione del testo
weight: 10
---

## Come fare:
```Arduino
String testo = "Ciao, mondo! Ciao!";
String vecchiaParola = "Ciao";
String nuovaParola = "Salve";

void setup() {
  Serial.begin(9600);
  // Mostra il testo originale
  Serial.println(testo);
  // Cerca e sostituisci
  testo.replace(vecchiaParola, nuovaParola);
  // Mostra il testo modificato
  Serial.println(testo);
}

void loop() {
  // Non fa nulla qui
}
```
Sample Output:
```
Ciao, mondo! Ciao!
Salve, mondo! Salve!
```

## Approfondimento
La ricerca e sostituzione di testo si basa su concetti degli anni '50 e '60, quando programmatori utilizzavano editor di testo per manipolare codice. Oggi esistono funzioni come `replace()` in Arduino, ma i linguaggi come Python o JavaScript offrono alternative potenti come le espressioni regolari. Arduino, semplice e diretto, permette sostituzioni base essenziali in dispositivi embedded e prototipazioni.

## Vedi anche
- [Arduino Reference: String Replace](https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/replace/)
- [Arduino Forum: Text Manipulation](https://forum.arduino.cc/)
- [Regular Expressions in Python](https://docs.python.org/3/library/re.html) per comprendere alternative più avanzate.
