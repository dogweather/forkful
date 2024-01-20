---
title:                "Wydobywanie podciągów"
html_title:           "Python: Wydobywanie podciągów"
simple_title:         "Wydobywanie podciągów"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/ruby/extracting-substrings.md"
---

{{< edit_this_page >}}

## Co i Dlaczego?

Wyciągając podciągi (substrings), pobieramy konkretny fragment tekstu z ciągu znaków. Robimy to, aby łatwiej manipulować danymi, analizować tekst, lub tworzyć nowe ciągi znaków.

## Jak to zrobić:

Ruby oferuje kilka metod do wyciągania podciągów. Zobaczmy kilka przykładów:

```Ruby
str = "Witaj, świecie programowania!"

str[7,9] 
# Zwróci "świecie"

str[0..4] 
# Zwróci "Witaj"

str.slice(16, 14) 
# Zwróci "programowania!"
```

## Pogłębione informacje

1) Kontekst historyczny: Wyciąganie podciągów jest często używane w różnych językach programowania, i Ruby to tradycję kontynuuje, oferując wiele pomocnych metod.

2) Alternatywy: Możesz też użyć `slice!` do usunięcia i zwrócenia podciągu lub `index` do znajdowania miejsca, gdzie zaczyna się konkretny podciąg.

3) Szczegóły implementacji: W Ruby indeksowanie zaczyna się od 0. Jeżeli użyjesz ujemnego indeksu, zaczyna on liczyć od końca ciągu.

## Zobacz też:

- Dokumentacja Ruby na temat metod ciągów znaków: https://ruby-doc.org/core-2.7.1/String.html
- Przewodnik po Ruby dla początkujących: https://www.ruby-lang.org/pl/documentation/quickstart/
- Zadania do ćwiczenia operacji na ciągach znaków w Ruby: https://www.w3resource.com/ruby-exercises/string/index.php