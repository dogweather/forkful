---
title:                "Porównywanie dwóch dat"
html_title:           "C++: Porównywanie dwóch dat"
simple_title:         "Porównywanie dwóch dat"
programming_language: "Bash"
category:             "Bash"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/bash/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Co i dlaczego?

Porównywanie dwóch dat to proste działanie, które sprawdza, która data jest wcześniejsza, późniejsza lub czy są one równe. Programiści często muszą to robić, aby kontrolować przepływ swoich skryptów lub programów, na przykład kontrolując ważność licencji lub ustalając osi czasu.

## Jak to zrobić:

Porównanie dwóch dat w Bashu jest dość proste. Ściśle rzecz biorąc, traktujemy je jako ciągi znaków i porównujemy je.

```Bash
data1=$(date -d "2022-05-01" +%s)
data2=$(date -d "2022-06-01" +%s)

if [ $data1 -eq $data2 ]; then
 echo "Daty są równe"
elif [ $data1 -lt $data2 ]; then
 echo "Data1 jest wcześniejsza"
else
 echo "Data2 jest wcześniejsza"
fi
```
Podczas wykonywania tego skryptu zobaczysz, która data jest wcześniejsza.

## Wgłębne informacje:

Bash to interpretowany język powłoki, który został wydany po raz pierwszy w 1989 roku. Wynika z niego wiele technik porównywania dat, ale ta metoda za pomocą stempli czasu jest jedną z najpowszechniejszych. Alternatywą może być użycie zewnętrznych narzędzi jak `awk` lub `perl`, ale po co komplikować, skoro Bash daje nam co potrzeba? Należy jednak pamiętać, że porównanie dat jako ciągów działa poprawnie tylko w przypadku formatów dat, które są porównywalne jako ciągi, takie jak YYYY-MM-DD.

## Zobacz również:

Możesz dowiedzieć się więcej o manipulacji datami w Bashu na stronach:
- [Advanced Bash-Scripting Guide: Chapter 16. Time/Date Operations](https://www.tldp.org/LDP/abs/html/timedate.html)
- [How to compare two dates in a shell script](https://stackoverflow.com/questions/34317857/how-to-compare-two-dates-in-a-shell-script)