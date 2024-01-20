---
title:                "Zamiana liter w ciągu na wielkie"
html_title:           "Arduino: Zamiana liter w ciągu na wielkie"
simple_title:         "Zamiana liter w ciągu na wielkie"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/arduino/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Co i dlaczego?

Zwiększanie wartości stringa jest procesem zmieniania wszystkich małych liter w stringach na duże litery. Programiści robią to, aby łatwo porównywać ciągi lub skoncentrować uwagę użytkownika na określonym tekście.

## Jak to zrobić:

```Arduino
String s = "witam";
s.toUpperCase();
```

Podczas wykonania kodu wejściowego `"witam"`, wyjście będzie wyglądało jak `"WITAM"`.

## Głębsze zrozumienie:

1. Historyczne zaplecze: Koncepcję zwiększania wartości stringów wprowadzono już na wczesnym etapie w językach programowania, aby ułatwić porównywanie ciągów i ułatwić odczytanie pewnych ciągów.

2. Alternatywy: W Arduino możesz także zamienić ciągi na małe litery, używając funkcji `toLowerCase()`.

3. Szczegóły implementacji: Metoda `toUpperCase()` jest wbudowana w bibliotekę łańcuchów Arduino. Przechodzi przez każdą literę w łańcuchu i zamienia ją na wielką literą, jeśli jest to litera mała.

## Zobacz także:
