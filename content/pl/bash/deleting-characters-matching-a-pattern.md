---
title:                "Usuwanie znaków pasujących do wzorca"
html_title:           "Bash: Usuwanie znaków pasujących do wzorca"
simple_title:         "Usuwanie znaków pasujących do wzorca"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/bash/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Co i Dlaczego?
Usuwanie znaków pasujących do wzorca jest jedną z funkcji, które programiści mogą wykonać w języku Bash. Polega to na wybraniu i usunięciu wszystkich wystąpień danego wzorca z tekstu. Jest to przydatne w wielu przypadkach, na przykład w filtracji danych lub przetwarzaniu tekstu.

## Jak to zrobić:
Bash jest językiem skryptowym, więc wbudowane polecenie sed może być wykorzystane do usuwania znaków pasujących do wzorca. Przykład może wyglądać następująco:
```
Teskst = "To jest przykładowy tekst."
echo $Tekst | sed 's/e//g'
```
Wyjście powinno wyglądać tak:
```
To jst przykładowy txt.
```
Można także wykorzystać wbudowane polecenie awk do osiągnięcia tego samego efektu. Przykład:
```
Tekst="To jest przykładowy tekst."
echo $Tekst | awk '{gsub("e","")}1'
```
Wyjście:
```
To jst przykładowy txt.
```

## Wgląd w głąb:
Usuwanie znaków pasujących do wzorca jest możliwe dzięki regularnym wyrażeniom, które są częścią składni języka Bash. Polecenie sed jest popularne w tym kontekście, ale istnieją także inne narzędzia, takie jak grep czy tr, które mogą być wykorzystane do tego celu. Implementacja jest oparta na algorytmie skanowania tekstu i usuwaniu znalezionych pasujących znaków.

## Zobacz też:
- [Bash Guide for Beginners](http://tldp.org/Ldp/Bash-Beginners-Guide/html/sect_04_02.html)
- [Regular Expression Tutorial](https://www.regular-expressions.info/tutorial.html)
- [UNIX Power Tools - Chapter 26: Manipulating Text Files](http://docstore.mik.ua/orelly/unix/upt/ch26_16.htm)