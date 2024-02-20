---
date: 2024-01-20 17:58:30.745140-07:00
description: "Szukanie i zamiana tekstu to fundament operacji na stringach, pozwalaj\u0105\
  cy zamieni\u0107 pewne wyra\u017Cenia czy ci\u0105gi znak\xF3w na inne. Programi\u015B\
  ci u\u017Cywaj\u0105 tego,\u2026"
lastmod: 2024-02-19 22:04:55.096694
model: gpt-4-1106-preview
summary: "Szukanie i zamiana tekstu to fundament operacji na stringach, pozwalaj\u0105\
  cy zamieni\u0107 pewne wyra\u017Cenia czy ci\u0105gi znak\xF3w na inne. Programi\u015B\
  ci u\u017Cywaj\u0105 tego,\u2026"
title: Wyszukiwanie i zamiana tekstu
---

{{< edit_this_page >}}

## Co i dlaczego?
Szukanie i zamiana tekstu to fundament operacji na stringach, pozwalający zamienić pewne wyrażenia czy ciągi znaków na inne. Programiści używają tego, żeby modyfikować dane, poprawiać błędy, a także przetwarzać i analizować kod czy tekst.

## Jak to zrobić:
W Ruby użyjemy metody `gsub` do wyszukiwania i zastępowania tekstu. Oto jak to działa:

```Ruby
tekst = "Witaj, świecie"
tekst.gsub!("świecie", "kodowanie")
puts tekst
```

Output:
```
Witaj, kodowanie
```

A teraz przykład z użyciem wyrażeń regularnych:

```Ruby
tekst = "ruby jest fajny. Ruby nauczy cię programować."
tekst.gsub!(/ruby/i, "Ruby")
puts tekst
```

Output:
```
Ruby jest fajny. Ruby nauczy cię programować.
```

## Głębiej w temat:
Szukanie i zamiana w tekstach to nie tylko podstawowe operacje, ale też ważna część skryptowania i pracy z dużymi zbiorami danych. Ruby przez lata udoskonalało swoje metody, jak `gsub` i `sub`, dostosowując je do różnorakich potrzeb. Alternatywnie, można używać innych gemów jak REXML do pracy z XML czy Nokogiri do HTML.

Metody takie jak `gsub` pozwalają nie tylko na proste zamiany tekstu, ale i na skomplikowane patyczki z użyciem tzw. wyrażeń regularnych. To potężne narzędzie do analizy i przetwarzania tekstu, które ma swoje korzenie w matematyce i teorii informacji.

## Zobacz również:
- Dokumentacja `String#gsub`: https://ruby-doc.org/core-2.7.0/String.html#method-i-gsub
- Przewodnik po wyrażeniach regularnych w Ruby: https://www.rubyguides.com/2015/06/ruby-regex/
- REXML: https://www.gemcutter.org/gems/rexml
- Nokogiri: https://nokogiri.org/
