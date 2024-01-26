---
title:                "Usuwanie znaków pasujących do wzorca"
date:                  2024-01-20T17:43:03.177930-07:00
model:                 gpt-4-1106-preview
simple_title:         "Usuwanie znaków pasujących do wzorca"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/python/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Co i Dlaczego?
Usuwanie znaków pasujących do wzorca to proces filtrowania tekstu według zdefiniowanych reguł, na przykład za pomocą wyrażeń regularnych. Robimy to, by usunąć niepożądane lub niepotrzebne informacje, zwiększając tym samym czytelność i poprawność danych.

## Jak to zrobić:
```Python
import re

# Przykład usuwania cyfr
tekst_z_liczbami = 'To jest przykład 123 z liczbami 456'
pattern = r'\d+' # Wzorzec dla cyfr
tekst_bez_liczb = re.sub(pattern, '', tekst_z_liczbami)
print(tekst_bez_liczb)

# Przykład usuwania znaków specjalnych
tekst_ze_znakami_specjalnymi = 'Hello! Jak się masz? Świetnie :) #python'
usun_znaki = r'[!?:)#]'
tekst_czysty = re.sub(usun_znaki, '', tekst_ze_znakami_specjalnymi)
print(tekst_czysty)
```

Output:
```
To jest przykład  z liczbami 
Hello Jak się masz Świetnie  python
```

## Deep Dive
Usuwanie znaków matching a pattern to częsta operacja w programowaniu, a korzenie ma w początkach informatyki. Istnieją różne podejścia, ale wyrażenia regularne (regex) to dziś standard. Regex umożliwia bardzo skomplikowane operacje na tekstach, co wykorzystywane jest w walidacji danych, wyszukiwaniu, etc. Alternatywą dla regex są funkcje wbudowane języków programowania jak `str.replace()` czy biblioteki takie jak `string`, lecz są mniej elastyczne.

Ważny szczegół: `re.sub()` używa żądłowego (eager) przetwarzania, tzn. znajdzie wszystkie wystąpienia wzorca. Można też użyć `re.finditer()` do leniwego (lazy) przeszukiwania tekstu, co może być efektywniejsze dla dużych danych.

## See Also
- Dokumentacja Pythona na temat modułu `re`: https://docs.python.org/3/library/re.html
- Interaktywny tutorial wyrażeń regularnych: https://regexone.com/
- Dokumentacja Pythona na temat stringów i metod operacji na nich: https://docs.python.org/3/library/string.html
