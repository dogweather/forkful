---
date: 2024-01-20 17:46:23.755713-07:00
description: "Jak to zrobi\u0107: ."
lastmod: '2024-03-13T22:44:35.919834-06:00'
model: gpt-4-1106-preview
summary: .
title: "Wycinanie pod\u0142a\u0144cuch\xF3w"
weight: 6
---

## Jak to zrobić:
```ruby
# Przykładowy string
tekst = "To jest przykładowy tekst."

# Użycie zakresów indeksów
podciag = tekst[3..7]
puts podciag  # Output: "jest"

# Użycie metody 'slice'
podciag2 = tekst.slice(14, 7)
puts podciag2  # Output: "tekst."

# Wykorzystanie wyrażeń regularnych
podciag3 = tekst[/przykładowy/]
puts podciag3  # Output: "przykładowy"
```

## Głębsze spojrzenie:
Ekstrakcja podciągów w Ruby ma proste i elastyczne podejście, co wyróżnia ten język na tle innych. Początki Ruby datuje się na początek lat 90., a z czasem ewoluował, oferując coraz więcej metod do manipulacji stringami. Alternatywy dla wyżej wymienionych metod to na przykład 'byteslice' czy używanie 'unpack'. Implikacja metod manipulacji stringami jest efektywna, ale należy pamiętać o różnicach między indeksowaniem a wyrażeniami regularnymi — różnią się one pod względem wydajności i elastyczności zastosowania.

## Zobacz również:
- [Ruby-Doc.org String](https://ruby-doc.org/core-2.7.0/String.html) – Dokumentacja klasy String w Ruby.
- [Regexp](https://ruby-doc.org/core-2.7.0/Regexp.html) – Dokumentacja na temat wyrażeń regularnych w Ruby.
