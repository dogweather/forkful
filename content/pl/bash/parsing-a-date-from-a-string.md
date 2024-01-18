---
title:                "Parsowanie daty ze stringa"
html_title:           "Bash: Parsowanie daty ze stringa"
simple_title:         "Parsowanie daty ze stringa"
programming_language: "Bash"
category:             "Bash"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/bash/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## Co i dlaczego?
Parsowanie daty z ciągu znaków to proces wyodrębniania informacji o dacie z tekstu. Programiści często wykonują tę czynność w swoich programach, aby przetworzyć dane otrzymane w formie tekstowej na bardziej czytelny i użyteczny format.

## Jak to zrobić:
```Bash
# Przykładowe wejście tekstowe: 12.03.2021
# Parsowanie daty w formacie Dzień.Miesiąc.Rok
date="12.03.2021"

# Wyciąganie dnia z tekstu
day=$(date -d "$date" +%d)
echo $day
# Output: 12

# Wyciąganie miesiąca z tekstu
month=$(date -d "$date" +%m)
echo $month
# Output: 03

# Wyciąganie roku z tekstu
year=$(date -d "$date" +%Y)
echo $year
# Output: 2021
```
W powyższym przykładzie wykorzystaliśmy polecenie `date` wraz z opcją `-d`, aby przekazać tekstową datę do parsowania. Następnie, za pomocą opcji `%d`, `%m` i `%Y` określamy, jakie elementy daty chcemy wyciągnąć z ciągu znaków.

## Głębsza analiza:
Parsowanie daty z ciągu znaków jest niezbędnym elementem w wielu programach, gdzie ważne jest przetworzenie danych wejściowych w czytelny i użyteczny format. Alternatywnym sposobem na to zadanie jest użycie narzędzi takich jak `awk` lub `sed`, jednak polecenie `date` jest popularnym wyborem ze względu na swoją prostotę i możliwość formatowania wyniku w wygodny sposób.

## Zobacz też:
- [Dokumentacja polecenia `date`](https://www.gnu.org/software/coreutils/manual/html_node/date-invocation.html)
- [Parsowanie daty w Bash za pomocą `awk`](https://linuxhint.com/parse_date_scripting_awk/)
- [Przykładowe skrypty Bash](https://www.shellscript.sh/functions.html)