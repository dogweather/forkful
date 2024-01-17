---
title:                "Znajdowanie długości ciągu znaków"
html_title:           "Bash: Znajdowanie długości ciągu znaków"
simple_title:         "Znajdowanie długości ciągu znaków"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/bash/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Co & Dlaczego?

Znajdowanie długości ciągu znaków to jedna z podstawowych operacji wykonywanych przez programistów. Polega to po prostu na policzeniu ilości znaków w danym ciągu. Jest to przydatne w wielu sytuacjach, na przykład w celu sprawdzenia, czy użytkownik wprowadził odpowiednią ilość znaków w formularzu lub w celu sprawdzenia, czy dany ciąg został poprawnie zaszyfrowany.

## Jak to zrobić:

Możemy wykorzystać polecenie ```wc -c``` w Bashu, aby znaleźć długość ciągu znaków. Przykładowa komenda może wyglądać tak:

```Bash
string="To jest przykładowy ciąg."
echo "Długość ciągu: $(echo -n $string | wc -c)"
```

To spowoduje wyświetlenie na ekranie liczby 26, ponieważ tyle znaków znajduje się w ciągu "To jest przykładowy ciąg."

## Głębsze zagadnienia:

Znajdowanie długości ciągu jest procesem prostym i powszechnie stosowanym w programowaniu. Wcześniej wykorzystywano inne sposoby, na przykład pętlę for w celu liczenia każdego pojedynczego znaku, ale polecenie ```wc -c``` jest bardziej wydajne i szybsze. Istnieją również inne sposoby liczenia długości ciągu, takie jak użycie funkcji strlen w języku C.

## Zobacz także:

- Dokumentacja polecenia ```wc``` w Bashu: https://www.gnu.org/software/coreutils/manual/html_node/wc-invocation.html
- Inne sposoby liczenia długości ciągu w różnych językach programowania: https://www.geeksforgeeks.org/length-of-a-string-using-recursion