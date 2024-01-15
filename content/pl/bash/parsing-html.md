---
title:                "Analiza składni HTML"
html_title:           "Bash: Analiza składni HTML"
simple_title:         "Analiza składni HTML"
programming_language: "Bash"
category:             "Bash"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/bash/parsing-html.md"
---

{{< edit_this_page >}}

## Dlaczego

Parsing HTML jest niezbędnym narzędziem dla wszystkich, którzy chcą analizować i przetwarzać zawartość stron internetowych. Pomaga on w ekstrakcji informacji oraz w automatyzacji różnych zadań związanych z analizą stron internetowych.

## Jak to zrobić

Aby parsować HTML za pomocą Bash, możemy skorzystać z narzędzi takich jak `grep` i `sed`. Na przykład, jeśli chcemy wydobyć wszystkie linki znajdujące się na stronie, możemy użyć następującego polecenia:
```Bash
curl <adres URL> | grep -o '<a [^>]*href=[^>]*>' | sed 's/<a [^>]*href=//g;s/>.*//g'
```
Powyższe polecenie pobiera zawartość strony internetowej za pomocą polecenia `curl`, a następnie używa `grep` i `sed` do wyodrębnienia wszystkich wystąpień znacznika `a` z atrybutem `href`. Następnie, używając wewnętrznego separatora `sed`, usuwa wszystko poza samym linkiem. Przykładowy wynik może wyglądać następująco:
```
http://example.com/
https://github.com/
https://www.google.com/
```

## Głębszy przegląd

Bash, mimo swojej prostoty, posiada wydajne narzędzia do przetwarzania tekstu, co czyni go dobrym wyborem do parsowania HTML. Większość poleceń wymienionych w dziale "Jak to zrobić" ma możliwość wykorzystania wyrażeń regularnych, co daje dużą elastyczność w wydobywaniu żądanych informacji. Należy jednak pamiętać, że Bash nie jest dedykowanym narzędziem do parsowania HTML i może nie być w stanie poprawnie obsłużyć skomplikowanych struktur dokumentów.

## Zobacz także

- [Platzi - Parsing HTML in Bash](https://platzi.com/blog/parsing-html-in-bash/)
- [IBM Developer - HTML parsing with Bash](https://developer.ibm.com/tutorials/l-html-bash/)
- [Awesome Shell - Parsing HTML](https://github.com/alebcay/awesome-shell#parsing-html)