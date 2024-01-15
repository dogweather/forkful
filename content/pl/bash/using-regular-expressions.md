---
title:                "Używanie wyrażeń regularnych"
html_title:           "Bash: Używanie wyrażeń regularnych"
simple_title:         "Używanie wyrażeń regularnych"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/bash/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Dlaczego?

Wielu programistów uważa regularne wyrażenia (ang. regular expressions) za niezbędne narzędzie w swoim arsenale. Poznaj kilka powodów, dla których warto nauczyć się ich używać.

## Jak to Zrobić?

Używanie regularnych wyrażeń w Bashu jest łatwe i intuicyjne. Wystarczy zapoznać się z kilkoma podstawowymi pojęciami i posiadać podstawową wiedzę na temat składni. Poniżej znajdziesz kilka przykładów kodu i odpowiadających im wyników, aby pomóc Ci zacząć.

```Bash
Polecenie: grep "kot" plik.txt
Wynik: W pliku.txt znajduje się wyraz "kot".
```

```Bash
Polecenie: grep -i "psa" plik.txt
Wynik: W pliku.txt znajduje się wyraz "Psa", pomimo że użyto małych liter.
```

```Bash
Polecenie: grep "^[A-Z]" plik.txt
Wynik: W pliku.txt znajdują się wszystkie linie rozpoczynające się dużymi literami.
```

```Bash
Polecenie: grep "[0-9]{3}-[0-9]{3}-[0-9]{4}" plik.txt
Wynik: W pliku.txt znajdują się wszystkie numery telefonów w formacie XXX-XXX-XXXX.
```

## Głębszy Zanurzanie Się

Regularne wyrażenia są potężnym narzędziem, które pozwala na precyzyjne wyszukiwanie i manipulowanie tekstem. Jest to szczególnie przydatne w skryptach Bash, gdzie możemy wykorzystać je do przetwarzania plików tekstowych. Pozwala nam to m.in. na:

- Wyszukiwanie określonych ciągów znaków w tekście z różnymi warunkami (np. z uwzględnieniem wielkości liter, liczby powtórzeń, itp.).
- Zastępywanie wybranych fragmentów tekstu innymi wyrażeniami.
- Wyciąganie i przetwarzanie danych z plików tekstowych na podstawie zdefiniowanych wzorców.

Warto również pamiętać, że każda implementacja regularnych wyrażeń jest nieco inna, więc dobrze jest zapoznać się z dokumentacją narzędzia, którego zamierzamy używać. Warto również ćwiczyć regularne wyrażenia w różnych narzędziach, aby być przygotowanym na różne scenariusze.

## Zobacz Również

- [Dokumentacja Bash](https://www.gnu.org/software/bash/manual/)
- [Oficjalne tutoriale regularnych wyrażeń](https://www.gnu.org/software/sed/manual/html_node/Regular-Expressions.html)
- [Praktyczny przewodnik po regularnych wyrażeniach w Bash](https://www.linux-magazine.com/Online/Features/Practical-Regex-for-the-Shell)