---
title:                "Znajdowanie długości ciągu znaków"
html_title:           "Arduino: Znajdowanie długości ciągu znaków"
simple_title:         "Znajdowanie długości ciągu znaków"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/bash/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Co i dlaczego?

Długość ciągu to ilość znaków, które go tworzą. Programiści często muszą znać tę długość, na przykład przy sortowaniu wymienności, walidacji danych wejściowych lub tworzeniu dynamicznych tablic.

## Jak to zrobić:

Możemy użyć wbudowanej funkcji Bash `${#string}` do znalezienia długości ciągu. Przykład:

```Bash
string="Witaj, Świecie!"
echo "Długość ciągu to: ${#string}"
```

Gdy uruchomisz powyższy skrypt, output będzie wyglądał tak:

```Bash
Długość ciągu to: 16
```

## Deep Dive:

Funkcja `${#string}` została wprowadzona w Bash 2.0 w 1997 roku jako jedna z kilku nowych funkcji operujących na ciągach znaków. Alternatywnie, możemy użyć polecenia `wc -m` do obliczenia długości ciągu, ale to zazwyczaj daje wynik o 1 większy, ponieważ liczy ono również nową linię. Technicznie rzecz biorąc, `${#string}` jest ekspansją parametru, a nie funkcją. Bash sprawdza literał po znaku '#', a jeśli jest to nazwa zmiennej, zwraca długość wartości zmiennej.

## Zobacz też:

1. [Advanced Bash-Scripting Guide: Manipulating Strings](http://tldp.org/LDP/abs/html/string-manipulation.html)
2. [GNU Bash Manual](https://www.gnu.org/software/bash/manual/bash.html)
3. [Stack Overflow: They're discussing the length of a string in Bash](https://stackoverflow.com/questions/17368067/length-of-string-in-bash)
4. [Unix & Linux Stack Exchange: Here, you can find more alternatives for finding the length of a string](https://unix.stackexchange.com/questions/19057/what-is-the-best-way-to-get-the-length-of-a-string-in-bash)