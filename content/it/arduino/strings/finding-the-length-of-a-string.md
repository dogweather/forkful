---
date: 2024-01-20 17:46:53.813008-07:00
description: "How to: (\"Come fare:\") Dal punto di vista storico, la funzione di\
  \ trovare la lunghezza di una stringa \xE8 sempre stata cruciale, poich\xE9 facilita\
  \ la\u2026"
lastmod: '2024-04-05T22:50:57.477689-06:00'
model: gpt-4-1106-preview
summary: "(\"Come fare:\") Dal punto di vista storico, la funzione di trovare la lunghezza\
  \ di una stringa \xE8 sempre stata cruciale, poich\xE9 facilita la manipolazione\
  \ di testi e dati."
title: Trovare la lunghezza di una stringa
weight: 7
---

## How to: ("Come fare:")
```Arduino
void setup() {
  Serial.begin(9600); // Avvio la comunicazione seriale
}

void loop() {
  String testo = "Ciao, mondo!";
  unsigned int lunghezza = testo.length(); // Trovo la lunghezza della stringa
  
  Serial.print("Lunghezza della stringa: ");
  Serial.println(lunghezza); // Stampo la lunghezza

  delay(1000); // Aspetto un secondo prima di ripetere
}
```

Output:
```
Lunghezza della stringa: 12
```

## Deep Dive ("Approfondimento")
Dal punto di vista storico, la funzione di trovare la lunghezza di una stringa è sempre stata cruciale, poiché facilita la manipolazione di testi e dati. In C, per esempio, si usa `strlen()` ma bisogna fare attenzione al carattere terminatore `\0`. In Arduino, `String.length()` rende le cose più facili e sicure, perché non lavora con array di caratteri ma con oggetti di tipo `String`, che gestiscono automaticamente la loro lunghezza. Se per motivi di performance o memoria si preferiscono le stringhe classiche (C-style strings), la funzione `strlen()` resta disponibile.

## See Also ("Vedi Anche")
- Documentazione ufficiale Arduino su `String`: https://www.arduino.cc/reference/en/language/variables/data-types/stringobject/
- Tutorial su `String` e `String functions`: https://www.arduino.cc/en/Tutorial/BuiltInExamples/StringLength
- Guida alla memoria e gestione delle stringhe: https://www.arduino.cc/en/Tutorial/BuiltInExamples/Memory
