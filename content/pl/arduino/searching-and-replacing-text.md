---
title:    "Arduino: Wyszukiwanie i podmienianie tekstu"
keywords: ["Arduino"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/arduino/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Dlaczego

Podczas programowania na Arduino często musimy zmieniać tekst w naszych programach. Jest to często uciążliwe, ale narzędzie do wyszukiwania i zastępowania tekstu (ang. search and replace) może znacznie ułatwić ten proces.

## Jak to zrobić

Wystarczy użyć funkcji `replace` wraz z odpowiednimi parametrami, aby dokonać zmian w tekście. Przykładowy kod może wyglądać następująco:

```Arduino
// Przykładowy tekst
String tekst = "Arduino jest super!";

// Zamienia słowo "super" na "fantastyczny"
tekst.replace("super", "fantastyczny");

// Wyświetlenie zmienionego tekstu
Serial.print("Tekst po zmianie: ");
Serial.println(tekst);

// Output: Tekst po zmianie: Arduino jest fantastyczny!
```

## Deep Dive

Funkcja `replace` przyjmuje dwa parametry - szukaną frazę oraz jej zamienioną wersję. Możliwe jest również użycie trzeciego parametru, aby podać początkowy indeks, od którego ma się rozpocząć zmiana tekstu. Możemy także użyć tej funkcji z obiektami typu `String` lub `char`.

W przypadku gdy chcemy dopasować i zmienić wiele wystąpień danej frazy, można użyć opcji "global search" poprzez dodanie flagi `g` do parametrów funkcji. Przykładowo:

```Arduino
// Przykładowy tekst
String tekst = "Super Arduino jest super!";

// Zamienia wszystkie wystąpienia słowa "super" na "fantastyczny"
tekst.replace("super", "fantastyczny", "g");

// Output: Fantasticzny Arduino jest fantastyczny!
```

## Zobacz także

- [Dokumentacja funkcji replace na stronie Arduino](https://www.arduino.cc/en/Reference/StringReplace)
- [Przykłady użycia funkcji replace w Arduino](https://www.programmingsimplified.com/arduino-replace-function)