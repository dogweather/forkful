---
title:                "Analiza składniowa daty z łańcucha znaków"
aliases:
- /pl/fish-shell/parsing-a-date-from-a-string/
date:                  2024-02-03T19:14:25.591683-07:00
model:                 gpt-4-0125-preview
simple_title:         "Analiza składniowa daty z łańcucha znaków"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/fish-shell/parsing-a-date-from-a-string.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Co i dlaczego?
Parsowanie daty ze stringa polega na ekstrahowaniu informacji o dacie zakodowanej w ciągach znaków i konwersji jej na strukturyzowany format, który środowiska programistyczne mogą rozpoznawać i manipulować. Programiści robią to, aby umożliwić operacje takie jak porównywanie dat, arytmetykę, formatowanie i lokalizację, które są niezbędne do efektywnego obsługiwania planowania, znaczników czasu i danych historycznych w oprogramowaniu.

## Jak to zrobić:
W Fish Shell nie masz wbudowanych poleceń specjalnie zaprojektowanych do parsowania dat z ciągów znaków. Zamiast tego, polegasz na zewnętrznych narzędziach takich jak `date` (dostępne w Linux i macOS) lub wykorzystujesz popularne narzędzia stron trzecich takie jak `GNU date` do bardziej skomplikowanego parsowania. Oto jak się za to zabrać:

**Korzystając z `date` w Fish:**

Aby przeanalizować ciąg daty w formacie "RRRR-MM-DD", możesz użyć polecenia `date` z opcją `-d` (lub `--date` dla GNU date) po której następuje string. Opcja `+` jest używana do formatowania wyjścia.

```fish
set date_str "2023-04-01"
date -d $date_str +"%A, %d %B %Y"
# Wyjście: Sobota, 01 Kwiecień 2023
```

Dla systemu macOS (który wymaga innego formatu dla flag `-j` i `-f`):

```fish
set date_str "2023-04-01"
date -j -f "%Y-%m-%d" $date_str +"%A, %d %B %Y"
# Wyjście: Sobota, 01 Kwiecień 2023
```

**Korzystając z GNU `date` do skomplikowanego parsowania:**

GNU `date` jest bardziej elastyczne w zakresie formatów ciągów. Może automatycznie wykrywać wiele wspólnych formatów ciągów dat bez konieczności jawnego określania formatu wejściowego:

```fish
set complex_date_str "1 Kwiecień 2023 14:00"
date -d "$complex_date_str" '+%Y-%m-%d %H:%M:%S'
# Wyjście: 2023-04-01 14:00:00
```

Jednak, przy pracy z ciągami dat, które mogą nie być automatycznie rozpoznawane lub gdy potrzebna jest precyzyjna kontrola nad formatem wejściowym, jawne określenie formatu wejściowego z GNU `date` nie jest bezpośrednio obsługiwane. W takich przypadkach rozważ przetworzenie wcześniejsze ciągu lub użycie innego narzędzia zaprojektowanego do bardziej skomplikowanych rutyn parsowania dat.
