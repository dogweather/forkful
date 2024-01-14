---
title:                "Fish Shell: Konwertowanie daty na ciąg znaków"
simple_title:         "Konwertowanie daty na ciąg znaków"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/fish-shell/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Dlaczego

Czasami w programowaniu musimy przekształcać daty w ciągi znaków. Może to być potrzebne, aby wyświetlić datę w odpowiednim formacie lub aby dokonać porównania z innymi datami w formie ciągu znaków. W tym blogu dowiecie się, jak można to zrobić w języku Fish Shell.

## Jak to zrobić

Aby przekształcić datę w ciąg znaków w języku Fish Shell, musimy użyć polecenia `date`. Przykładowe użycie polecenia wygląda następująco:

```Fish Shell
date "+%Y-%m-%d"
```

Output:
```2000-05-28```

Możemy również użyć polecenia `strftime` aby określić niestandardowy format daty. Przykładowe użycie wygląda tak:

```Fish Shell
strftime "%d %b %Y" (date -d "10 days ago")
```

Output:
```18 Sep 2021```

Pamiętajmy, że wiele innych poleceń w języku Fish Shell może przyjmować datę jako argument i zwrócić ją w formie ciągu znaków.

## Deep Dive

Podczas przekształcania daty w ciąg znaków warto zwrócić uwagę na kilka ważnych rzeczy. Po pierwsze, powinniśmy zawsze uwzględnić strefę czasową lub użyć polecenia `date -u` aby uzyskać czas w uniwersalnej strefie czasowej.

Kolejną ważną rzeczą jest obsługa różnych formatów daty, które mogą występować w różnych systemach lub lokalizacjach. Używając polecenia `strftime` możemy określić dowolny niestandardowy format, który jest łatwy do odczytania dla użytkownika.

Podczas przekształcania daty w ciąg znaków należy również pamiętać o wyświetlaniu jej w odpowiedniej strefie czasowej dla naszych użytkowników lub zamieniać strefę czasową na czytelny dla nich format.

## Zobacz również

- Dokumentacja Fish Shell: [https://fishshell.com/docs/current/](https://fishshell.com/docs/current/)
- Przekształcanie daty i czasu w języku Fish Shell: [https://fishshell.com/docs/current/cmds/date.html](https://fishshell.com/docs/current/cmds/date.html)
- Przykłady użycia polecenia `strftime`: [https://fishshell.com/docs/current/cmds/strftime.html](https://fishshell.com/docs/current/cmds/strftime.html)