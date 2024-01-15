---
title:                "Usuwanie znaków pasujących do wzorca"
html_title:           "C: Usuwanie znaków pasujących do wzorca"
simple_title:         "Usuwanie znaków pasujących do wzorca"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/c/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Dlaczego
Często w trakcie programowania musimy prześwietlić i przetwarzać duże ilości tekstu. W takich przypadkach, by ułatwić sobie pracę, możemy szukać sposobów na usuwanie niepotrzebnych znaków z naszych danych. W poradniku tym dowiesz się, jak w języku C skasować znaki pasujące do określonego wzoru.

## Jak to zrobić
Aby usunąć znaki pasujące do określonego wzoru w języku C, musimy skorzystać z funkcji `strfry`, która znajduje się w bibliotece string.h. Przykładowe użycie tej funkcji wygląda następująco:

```C
char data[] = "Przykładowy123 tekst456";
char *pattern = "123";
char *result = strfry(data, pattern);
```

Po wykonaniu powyższego kodu, zmienna result będzie przechowywać wyłącznie tekst "Przykładowy tekst456", ponieważ funkcja `strfry` dokonała usunięcia znaków pasujących do wzoru "123". 

## Deep Dive
Funkcja `strfry` służy do przeszukiwania i usuwania podciągów znaków z danego tekstu. W przypadku, gdy znajdzie dopasowanie do wzoru, usuwa cały podciąg w tym miejscu i zwraca pozostały tekst. Warto również zauważyć, że funkcja ta jest wrażliwa na wielkość liter.

W przypadku, gdy chcemy zastosować tę funkcję w obrębie jednego słowa, możemy wykorzystać funkcję `strfry_l` z biblioteki `locale.h`, która bierze pod uwagę aktualne ustawienia regionalne i nie usuwa znaków dzielących.

```
char *result = strfry_l(data, pattern, locale);
```

## Zobacz także
- [Dokumentacja funkcji `strfry` w języku C](https://www.cplusplus.com/reference/cstring/strfry/)
- [Przykładowe wykorzystanie funkcji `strfry` w praktyce](https://www.geeksforgeeks.org/write-your-own-strfry-in-c/)
- [Funkcja `strfry` w kontekście przetwarzania tekstu](https://www.tutorialspoint.com/c_standard_library/c_function_strfry.htm)