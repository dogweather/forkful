---
title:                "Bash: Zmiana tekstu na wielkie litery"
programming_language: "Bash"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/bash/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Dlaczego

Cześć czytelnicy, w dzisiejszym poście opowiemy o jednej prosty, ale przydatnej czynności w Bash - jak zamienić pierwszą literę w zdaniu na wielką. Nie tylko ułatwi to czytelnosć kodu, ale także może być przydatne przy wyświetlaniu wiadomości użytkownikowi. Czytaj dalej, aby dowiedzieć się dlaczego warto to zrobić.

## Jak to zrobić

W Bash mamy dostęp do kilku wbudowanych funkcji, które pomogą w zamianie pierwszej litery na wielką. Jedną z nich jest `ucfirst()`, która zwraca pierwszą literę w zdaniu zapisaną wielką literą.

```Bash
#!/bin/bash

str="witaj świecie!"
echo "${str^}"
```

```
Witaj świecie!
```

Możemy również użyć `tr` do zamiany pierwszej litery na wielką. Ta metoda jest przydatna, jeśli chcemy zamienić pierwsze litery we wszystkich słowach w zdaniu na wielkie.

```Bash
#!/bin/bash

str="witaj świecie!"
echo "$str" | tr 'a-z' 'A-Z'
```

```
WITAJ ŚWIECIE!
```

Jeśli potrzebujemy zamienić tylko pierwsze litery we wszystkich słowach na wielkie, możemy użyć `sed`.

```Bash
#!/bin/bash

str="witaj świecie!"
echo "$str" | sed 's/\b\(.\)/\u\1/g'
```

```
Witaj Świecie!
```

## Deep Dive

Zamiana pierwszej litery na wielką może być pomocna przy wyświetlaniu komunikatów użytkownikowi w czytelny sposób, szczególnie jeśli korzystamy z `read` do przyjmowania danych od użytkownika.

Możemy również dostosować zamianę pierwszej litery na wielką do własnych potrzeb, na przykład zmieniać tylko litery w danym zakresie ASCII lub ignorować niektóre wyjątki.

Zapoznanie się z dokumentacją Bash i eksperymentowanie z różnymi metodami może pomóc w zrozumieniu i wykorzystaniu zamiany pierwszej litery na wielką w różnych przypadkach.

## Zobacz też

- [Dokumentacja Bash](https://www.gnu.org/software/bash/manual/bash.html)
- [Funkcje tekstowe w Bash](https://www.baeldung.com/linux/bash-text-processing-functions)
- [Manipulowanie tekstem w Bash](https://www.linuxjournal.com/content/bash-string-manipulation)