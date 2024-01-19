---
title:                "Analiza składniowa daty z ciągu znaków"
html_title:           "Clojure: Analiza składniowa daty z ciągu znaków"
simple_title:         "Analiza składniowa daty z ciągu znaków"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/fish-shell/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## Co i dlaczego?
Przetwarzanie daty z ciągu znaków to odzyskiwanie tekstu i przekształcanie go w postać daty, która można łatwo obsłużyć w programie. Programiści robią to, aby uprościć obsługę dat w swoim kodzie i zwiększyć czytelność danych.

## Jak to zrobić:
Rzut oka na kod pokazuje, jak łatwo możemy parsować datę z ciągu znaków w Fish Shell:

```fish
set -l str_date "2021-12-12"
set -l parsed_date (date -u -j -f "%Y-%m-%d" $str_date "+%Y %m %d")
echo $parsed_date
```
Na wyjściu powinno pojawić się:

```fish
2021 12 12
```

## Głębsze zanurzenie
Przetwarzanie daty z tekstu to technika, którą programiści stosują od dawna, sama koncepcja istnieje odkąd powstały pierwsze języki programowania. W Fish Shell mamy prosty i bezpośredni sposób na przetwarzanie tekstu danej daty, ale warto pamiętać, że są też inne narzędzia i techniki, które można zastosować, w zależności od konkretnych wymagań.

Date, klasa wbudowana Fish'a, to proste i funkcjonalne narzędzie do przetwarzania dat, dzięki któremu nie musimy polegać na zewnętrznych bibliotekach, ale jeśli Twoje oczekiwania są bardziej skomplikowane, może ono nie spełnić Twoich wymagań.

Co więcej, niektóre implementacje wymagają uwzględnienia różnic czasowych między strefami, co może skomplikować sprawę. Dlatego zawsze warto zrozumieć swoje wymagania zanim zdecydujesz się na konkretny sposób przetwarzania daty.

## Zobacz także:
-[Dokumentacja Fish Shell](https://fishshell.com/docs/current/index.html)
-[Przewodnik po funkcji Date w Fish Shell](https://www.tecmint.com/date-command-in-linux/)
-[Poradnik Tech Republic's How-To do przetwarzania daty](https://www.techrepublic.com/article/how-to-use-the-linux-date-command-to-do-more-than-just-tell-time/)