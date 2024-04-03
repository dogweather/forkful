---
date: 2024-01-20 17:43:19.421181-07:00
description: "Usuwanie znak\xF3w pasuj\u0105cych do wzorca to filtrowanie string\xF3\
  w przez wykrywanie i wyeliminowanie niechcianych sekwencji. Programi\u015Bci robi\u0105\
  \ to, by oczy\u015Bci\u0107\u2026"
lastmod: '2024-03-13T22:44:35.915122-06:00'
model: gpt-4-1106-preview
summary: "Usuwanie znak\xF3w pasuj\u0105cych do wzorca to filtrowanie string\xF3w\
  \ przez wykrywanie i wyeliminowanie niechcianych sekwencji."
title: "Usuwanie znak\xF3w pasuj\u0105cych do wzorca"
weight: 5
---

## How to: (Jak to zrobić?)
```Ruby
# Usuwanie wszystkich cyfr z tekstu
text = "Ruby ver. 3.1.0 - 2023 release"
cleaned_text = text.gsub(/[0-9]/, '')
puts cleaned_text
# Output: Ruby ver. . -  release

# Usuwanie tylko określonych znaków (np. kropek i myślników)
specific_chars_removed = text.gsub(/[.-]/, ' ')
puts specific_chars_removed
# Output: Ruby ver  3 1 0   2023 release

# Użyj `delete` do usunięcia konkretnych znaków
no_dashes = text.delete('-')
puts no_dashes
# Output: Ruby ver. 3.1.0  2023 release
```

## Deep Dive (Dogłębna analiza)
Usuwanie znaków według wzorca nie jest nowością. Narzędzia takie jak `sed` w systemach Unix już dawno oferowały podobne funkcje. W Ruby, metoda `gsub` jest mocno rozbudowana i pozwala używać wyrażeń regularnych do określania wzorców, które chcemy usunąć. Alternatywnie, `delete` jest prosta, ale usuwa tylko konkretnie określone znaki.

Ruby używa wyrażeń regularnych kompatybilnych z biblioteką `Onigmo`, która rozwijała się od standardów POSIX i Perl. Pozwala to na bardzo elastyczne wzorce. W praktyce oznacza to, że możliwości edycji stringów są ogromne, ograniczone tylko wyobraźnią programisty.

Niekiedy lepszym wyborem może być `tr`, gdy chcemy zamienić jeden zestaw znaków na inny. `gsub` i `delete` operują na kopiach stringa, co może być istotne przy bardzo dużych danych ze względu na zużycie pamięci.

## See Also (Zobacz także)
- [Ruby API Documentation for `String#gsub`](https://ruby-doc.org/core-3.1.0/String.html#method-i-gsub)
- [Ruby API Documentation for `String#delete`](https://ruby-doc.org/core-3.1.0/String.html#method-i-delete)
- [Ruby Quicktips - Regular Expressions](https://www.rubyguides.com/2015/06/ruby-regex/)
- [Onigmo Regular Expressions Library](https://github.com/k-takata/Onigmo)
