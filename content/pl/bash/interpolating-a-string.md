---
title:                "Interpolacja ciągu znaków"
html_title:           "C++: Interpolacja ciągu znaków"
simple_title:         "Interpolacja ciągu znaków"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/bash/interpolating-a-string.md"
---

{{< edit_this_page >}}

## Co i dlaczego?

Interpolacja łańcuchów to metoda wstawiania wartości zmiennych do łańcuchów tekstowych. Programiści używają jej, aby stworzyć dynamiczne wiadomości czy instrukcje.  

## Jak to zrobić:

W Bashu zmienną interpolujemy umieszczając ją w nawiasach klamrowych po znaku dolara. Na przykład:

```Bash
imie="Jan"
echo "Cześć, ${imie}"
```

Wyjście:

```Bash
Cześć, Jan
```

## Pogłębienie:

Interpolacja łańcuchów jest powszechna w wielu językach programowania. Jako jedna z podstawowych operacji na łańcuchach, jest z nami od dawna. Alternatywą dla interpolacji jest konkatenacja, ale jest mniej czytelna i zależna od kontekstu.

Główne różnice w implementacji między językami są składniowe. Np. Python używa f-stringi, JavaScript - template strings. Bash zaś korzysta ze znaku dolara i nawiasów klamrowych.

## Zobacz także:

- Bash: String Manipulations (https://tldp.org/LDP/abs/html/string-manipulation.html)
- Practical Bash Programming (https://www.tldp.org/HOWTO/Bash-Prog-Intro-HOWTO.html#toc7)
- StackOverflow: What is string interpolation in Bash? (https://stackoverflow.com/questions/415677/how-to-replace-placeholders-in-a-text-file)