---
title:                "Korzystanie z wyrażeń regularnych"
html_title:           "Bash: Korzystanie z wyrażeń regularnych"
simple_title:         "Korzystanie z wyrażeń regularnych"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/bash/using-regular-expressions.md"
---

{{< edit_this_page >}}

## What & Why? (Co i dlaczego?)
Regularne wyrażenia to wzorce, które pomagają w przeszukiwaniu tekstu, sprawdzaniu formatu i manipulacji danymi. Programiści używają ich, by szybko i efektywnie obrabiać teksty i dane.

## How to: (Jak to zrobić?)
```Bash
# Wyszukiwanie słowa 'baz' w pliku 'foo.txt':
grep 'baz' foo.txt

# Zamiana 'http' na 'https' we wszystkich plikach .txt:
sed -i 's/http/https/g' *.txt

# Wyciągnięcie samych numerów z pliku (np. '123'):
grep -o '[0-9]\+' plik.txt

# Przykładowe wyjście pokazujące znalezione numery
123
256
789
```

## Deep Dive (Dogłębna analiza)
Regularne wyrażenia, znane jako *regex*, narodziły się w latach 50. XX wieku. Współczesne implementacje bazują na notacjach Perl i POSIX. Alternatywy dla *regex* mogą obejmować parserowanie za pomocą dedykowanych bibliotek językowych. W Bashu głównie używa się *grep* do wyszukiwania i *sed* do manipulacji tekstami za pomocą regularnych wyrażeń.

## See Also (Zobacz również)
- [GNU Grep Documentation](https://www.gnu.org/software/grep/manual/grep.html)
- [GNU Sed Documentation](https://www.gnu.org/software/sed/manual/sed.html)
- [Regex101: Online regex tester and debugger](https://regex101.com/)
- [Regular Expressions Info](https://www.regular-expressions.info/)