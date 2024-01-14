---
title:    "Fish Shell: Otrzymywanie aktualnej daty"
keywords: ["Fish Shell"]
---

{{< edit_this_page >}}

## Dlaczego?

Dlaczego ktoś zechciałby poznać bieżącą datę? Wielu programistów potrzebuje bieżącej daty do wykonywania różnych zadań, takich jak:
- tworzenie plików lub folderów z bieżącą datą w nazwie
- określanie, czy wydarzenie ma miejsce w przyszłości czy w przeszłości
- wyświetlanie bieżącej daty w aplikacji lub na stronie internetowej
Z pomocą Fish Shell możesz łatwo pobrać bieżącą datę i wykorzystać ją do tych i innych zadań.

## Jak to zrobić?

Aby uzyskać bieżącą datę w Fish Shell, musisz użyć wbudowanej w system komendy `date`. Istnieje wiele różnych opcji tej komendy, ale najprostsza z nich to `date +%d/%m/%Y`, która zwraca datę w formacie "DD/MM/RRRR". Możesz także zmienić ten format na dowolny, wykorzystując specjalne znaki określające różne elementy daty i czasu. Na przykład `date '+%Y-%m-%d %H:%M:%S'` zwróci bieżącą datę i godzinę w formacie "RRRR-MM-DD GG:MM:SS".

Przykładowy kod:

```Fish Shell
date +%d/%m/%Y
```

Wyjście:

```
20/04/2021
```

## Dogłębny przegląd

Pobieranie bieżącej daty w Fish Shell opiera się na wbudowanej komendzie `date`, która jest dostępna w systemach operacyjnych opartych na Linuxie oraz w systemach macOS. Ta komenda jest również bardzo użyteczna, ponieważ pozwala na manipulowanie różnymi elementami daty i czasu, takimi jak dodawanie lub odejmowanie dni, godzin czy miesięcy.

Powyższe wyjaśnione formaty nie stanowią wyczerpującej listy, ponieważ istnieje wiele innych specjalnych znaków, które można wykorzystać do tworzenia niestandardowych formatów daty. Warto też wspomnieć, że komenda `date` pozwala także na wyświetlanie daty w formie timestamp (czasu w sekundach od 1 stycznia 1970 roku).

## Zobacz także

- [Dokumentacja Fish Shell: polecenie date](https://fishshell.com/docs/current/cmds/date.html)
- [Przewodnik po wykorzystaniu komendy date](https://www.linode.com/docs/guides/bash-date-command-reference/)
- [Blog o programowaniu w języku polskim](https://piecioshka.pl/)