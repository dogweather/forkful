---
date: 2024-01-20 17:44:57.267112-07:00
description: "How to: Do wydobycia pod\u0142a\u0144cuch\xF3w w Arduino s\u0142u\u017C\
  y metoda `substring()`. Poni\u017Cej znajdziesz przyk\u0142ady u\u017Cycia."
lastmod: '2024-03-13T22:44:35.661376-06:00'
model: gpt-4-1106-preview
summary: "Do wydobycia pod\u0142a\u0144cuch\xF3w w Arduino s\u0142u\u017Cy metoda\
  \ `substring()`."
title: "Wycinanie pod\u0142a\u0144cuch\xF3w"
weight: 6
---

## How to:
Do wydobycia podłańcuchów w Arduino służy metoda `substring()`. Poniżej znajdziesz przykłady użycia:

```Arduino
String tekst = "Witaj, Świat!";
String powitanie = tekst.substring(0, 6); // Wydobycie "Witaj,"
String reszta = tekst.substring(7); // Wydobycie "Świat!"

void setup() {
  Serial.begin(9600);
  Serial.println(powitanie); // Wyświetli "Witaj,"
  Serial.println(reszta); // Wyświetli "Świat!"
}

void loop() {
  // Tutaj nic nie umieszczamy.
}
```

## Deep Dive
Wycinanie podłańcuchów istnieje od początków programowania. W Arduino, metoda `substring()` jest wbudowana w klasę `String`, ułatwiając operacje na tekstach. Alternatywą może być użycie operacji na tablicach znaków (C-style strings), które zazwyczaj są szybsze ale bardziej skomplikowane w implementacji. Implementacja `substring()` w Arduino tworzy nowy obiekt `String`, który może wpływać na wydajność i zużycie pamięci, zwłaszcza w większych aplikacjach.

## See Also
Informacje na temat klas i metod w Arduino znajdziesz w oficjalnej dokumentacji online:
- Arduino `String` class reference: [https://www.arduino.cc/reference/en/language/variables/data-types/stringobject/](https://www.arduino.cc/reference/en/language/variables/data-types/stringobject/)
- Dodatkowe porady i triki dotyczące pracy z tekstami: [https://www.arduino.cc/en/Tutorial/BuiltInExamples](https://www.arduino.cc/en/Tutorial/BuiltInExamples)
