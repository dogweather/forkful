---
title:                "Analiza składniowa daty z ciągu znaków"
html_title:           "Clojure: Analiza składniowa daty z ciągu znaków"
simple_title:         "Analiza składniowa daty z ciągu znaków"
programming_language: "Bash"
category:             "Bash"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/bash/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## Co i dlaczego?

Analiza daty z ciągu to proces, w którym ciąg zawierający informacje o dacie i godzinie jest konwertowany na datę. Programiści robią to, żeby łatwo manipulować i porównywać daty oraz godziny.

## Jak to zrobić:

Możemy skorzystać z wbudowanej komendy Bash `date -d`. Zobacz poniżej:

```Bash
date_string="2022-03-01 23:59:59"
date_in_seconds=$(date -d "$date_string" +%s)
echo $date_in_seconds
```
Po uruchomieniu powyższego kodu, wydrukowany zostanie odpowiednik danej daty w sekundach, od 1 stycznia 1970 roku (początek ery UNIX).

## Zagłębianie się 

Analiza daty z ciągu rozpoczęła się prawdopodobnie wraz z powstawaniem pierwszych języków programowania. Skonstruowanie odpowiedniego formatu do przechowywania daty i czasu jest kluczowe dla prawidłowego funkcjonowania aplikacji i systemów.

Jako alternatywę dla 'date -d', możemy użyć `date -jf` w systemach Unixowych, gdzie 'j' oznacza „Julian date”, a 'f' umożliwia precyzyjne określenie formatu wejściowego.

Podczas analizy, data jest najpierw konwertowana na sekundy (od początku ery Unix), co umożliwia porównywanie różnych dat. Obie metody zwracają datę jako liczbę sekund, co pozwala na łatwe porównywanie i manipulację danymi.

## Zobacz także:

- Szczegółowa dokumentacja komendy `date`: https://www.gnu.org/software/coreutils/manual/html_node/date-invocation.html
- Kurs na temat analizy daty z ciągu w Bash: https://www.tutorialkart.com/bash-shell-scripting/bash-date-format-options/
- Dodatkowe informacje na temat daty w Julianskim formacie: https://en.wikipedia.org/wiki/Julian_day