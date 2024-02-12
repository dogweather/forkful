---
title:                "Analiza składniowa daty z łańcucha znaków"
aliases:
- /pl/bash/parsing-a-date-from-a-string/
date:                  2024-02-03T19:14:11.225821-07:00
model:                 gpt-4-0125-preview
simple_title:         "Analiza składniowa daty z łańcucha znaków"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/bash/parsing-a-date-from-a-string.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Co i dlaczego?

Analiza daty ze stringa w Bashu polega na ekstrakcji i konwersji informacji daty z danych tekstowych na format, który Bash może manipulować lub używać do dalszych procesów. Jest to często wymagane w skryptach do takich zadań, jak analiza plików logów, organizacja plików na podstawie znaczników daty czy automatyczne raportowanie, co czyni to istotną umiejętnością dla programistów do skutecznego zarządzania i wykorzystywania danych czasowych.

## Jak to zrobić:

Sam Bash jest dość ograniczony w bezpośrednich możliwościach analizy dat, często polegając na zewnętrznych narzędziach takich jak `date` i `awk` do bardziej zaawansowanej manipulacji. Oto jak można przeanalizować określony format, a następnie użyć go z poleceniem `date` do konwersji lub wykonania operacji.

**Przykład 1:** Wyodrębnij string daty i przekształć go na inny format.

Załóżmy, że masz datę w formacie `rrrr-mm-dd` i chcesz ją przekształcić na `dd-mm-rrrr`.

```bash
original_date="2023-04-01"
formatted_date=$(date -d $original_date '+%d-%m-%Y')

echo $formatted_date
```

**Przykładowy wynik:**
```
01-04-2023
```

Używa to polecenie `date` z opcją `-d` do określenia wejściowego stringa daty i `+%d-%m-%Y` do formatowania wyniku.

**Przykład 2:** Użycie `awk` do wyodrębnienia daty z uporządkowanego tekstu logu i jej przekształcenia.

Załóżmy, że masz linię pliku logu:

```
2023-04-01 12:00:00 User logged in
```

Możesz wyodrębnić i przekształcić część daty używając `awk` i `date`.

```bash
log_line="2023-04-01 12:00:00 User logged in"
date_part=$(echo $log_line | awk '{print $1}')
formatted_date=$(date -d $date_part "+%A, %B %d, %Y")

echo $formatted_date
```

**Przykładowy wynik:**
```
Sobota, Kwiecień 01, 2023
```

Ten przykład używa `awk` do podzielenia linii logu i wyodrębnienia części daty (`$1` reprezentuje pierwsze pole rozdzielone spacją), a następnie `date` jest używany do jej ponownego sformatowania.

### Użycie narzędzi stron trzecich

Do bardziej złożonej analizy lub przy radzeniu sobie z szeroką gamą formatów dat, bardzo przydatne mogą być narzędzia stron trzecich, takie jak `dateutils`.

**Przykład z `dateutils`:**

Załóżmy, że masz string daty w niestandardowym formacie, na przykład `Kwiecień 01, 2023`.

```bash
original_date="Kwiecień 01, 2023"
formatted_date=$(dateconv -i "%B %d, %Y" -f "%Y-%m-%d" <<< $original_date)

echo $formatted_date
```

**Przykładowy wynik:**
```
2023-04-01
```

To polecenie korzysta z `dateconv` z `dateutils`, określając format wejściowy za pomocą `-i` i pożądany format wyjściowy za pomocą `-f`. `dateutils` obsługuje ogromny zakres formatów dat i czasu, co czyni go bardzo wszechstronnym narzędziem do zadań związanych z analizą daty w skryptach Bash.
