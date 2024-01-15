---
title:                "Uzyskiwanie aktualnej daty"
html_title:           "Fish Shell: Uzyskiwanie aktualnej daty"
simple_title:         "Uzyskiwanie aktualnej daty"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/fish-shell/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Dlaczego?

Dlaczego ktoś chciałby otrzymać aktualną datę? Jest to przydatne do wielu różnych zastosowań, takich jak tworzenie plików lub katalogów z datą jako ich nazwą, automatyczne tworzenie dziennych wpisów lub raportów, lub nawet prosty sposób na upewnienie się, że pliki są odpowiednio zaktualizowane.

## Jak To Zrobić

```Fish Shell``` posiadają wbudowane polecenie ```date```, które zwraca aktualną datę w formacie ```MM/DD/RRRR```. Możesz wpisać jedynie polecenie ```date``` i naciśnij Enter, aby wyświetlić bieżącą datę, lub możesz użyć opcji, aby dostosować wyjście.

```Fish Shell
date +%d/%m/%Y
```

Powyższe polecenie zwróci datę w formacie ```DD/MM/RRRR```. Możesz zmienić format wyjścia, korzystając z różnych opcji, takich jak ```%H``` dla godziny, ```%M``` dla minuty lub ```%S``` dla sekundy. Aby uzyskać pełną listę opcji, wpisz ```man date``` w terminalu.

## Głębsza Analiza

Polecenie ```date``` korzysta z ustawień lokalnych w celu określenia formatu wyjścia. Jeśli chcesz zmienić domyślny format daty, możesz zmienić ustawienia regionalne swojego systemu. W systemach Unix/Linux możesz to zrobić za pomocą polecenia ```locale```. W systemie MacOS możesz użyć narzędzia ```defaults```. Więcej informacji na temat zmiany ustawień regionalnych możesz znaleźć w dokumentacji systemu operacyjnego.

## Zobacz też

- [Dokumentacja Polecenia Date](https://fishshell.com/docs/current/commands.html#date)
- [Poradnik z użyciem polecenia "date" w bash](https://linuxize.com/post/bash-date-command/)
- [Zmiana ustawień regionalnych w systemie MacOS](https://developer.apple.com/library/archive/documentation/MacOSX/Conceptual/BPInternational/LanguageandLocaleIDs/LanguageandLocaleIDs.html)