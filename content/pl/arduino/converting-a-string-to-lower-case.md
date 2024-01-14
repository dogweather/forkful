---
title:    "Arduino: Konwersja ciągu znaków na małe litery."
keywords: ["Arduino"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/arduino/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Dlaczego

Zamiana stringa na małe litery jest przydatną funkcją w programowaniu Arduino, ponieważ pozwala na porównywanie i analizowanie tekstu bez uwzględniania wielkości liter. Jest to szczególnie ważne w przypadku interakcji z użytkownikiem, gdzie wprowadzony tekst może mieć różne wersje z powodu niedokładnego wpisania. 

## Jak to zrobić

Aby zamienić string na małe litery w Arduino, należy wykorzystać funkcję ```toLowerCase()```. Poniższy kod przedstawia przykładową implementację:

```Arduino
void setup() {
  Serial.begin(9600);                   // Inicjalizacja połączenia z komputerem
}

void loop() {
  String tekst = "PrZyKłAdoWy TEKST";   // Przykładowy tekst do konwersji
  String tekst_male = tekst.toLowerCase(); // Wywołanie funkcji toLowerCase()
  Serial.println(tekst_male);           // Wyświetlenie zmienionego tekstu
}
```

Output:

```
przykładowy tekst
```

## Wgłębienie się w temat

Konwersja na małe litery jest często wykonywana w celu ułatwienia porównania i analizy tekstu. Funkcja ```toLowerCase()``` w Arduino obsługuje kod ASCII, co oznacza, że niektóre znaki diakrytyczne mogą nie zostać poprawnie przetworzone. W przypadku konieczności uwzględnienia znaków specjalnych, warto skorzystać z bibliotek zewnętrznych, takich jak [Arduino String Functions] (https://github.com/greiman/Arduino-string-utilities).

## Zobacz także

- [Referencja funkcji toLowerCase() w Arduino] (https://www.arduino.cc/reference/en/language/functions/string/characters/tolowercase/)
- [Zamiana liter na małe z polskimi znakami w programie Arduino] (https://programuj.ninja/arduino-podstawy-robota-piasty-sp1/)
- [Manipulowanie tekstem w Arduino za pomocą funkcji String] (http://tesla4you.pl/zamiany-w-arduino-zacinamy-delikatna-materie-funkcja-string-part1)