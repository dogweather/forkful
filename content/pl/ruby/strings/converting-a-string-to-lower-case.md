---
date: 2024-01-20 17:39:11.663863-07:00
description: "How to: (Jak to zrobi\u0107:) ."
lastmod: '2024-04-05T21:53:37.349874-06:00'
model: gpt-4-1106-preview
summary: ''
title: "Konwersja ci\u0105gu znak\xF3w na ma\u0142e litery"
weight: 4
---

## How to: (Jak to zrobić:)
```Ruby
# Przykład konwersji stringa na małe litery w Ruby
original_string = "Jestem WIELKI napis!"
lowercase_string = original_string.downcase

puts lowercase_string
# Output: jestem wielki napis!
```

## Deep Dive (Dogłębna Analiza)
Ruby od zawsze dba o to, by operacje na stringach były proste i intuicyjne. Metoda `.downcase` istnieje w Ruby od początku jej istnienia i jest wykorzystywana do konwersji stringów na małe litery. Alternatywą dla `.downcase` jest `.downcase!`, która bezpowrotnie zmienia oryginalny string, a nie tylko zwraca jego kopię.

W Ruby, metoda `.downcase` dotyczy Unicode, co oznacza, że radzi sobie nie tylko z literami ASCII, ale również z literami w wielu różnych systemach pisma, jak łacińskim, greckim, cyrylicą, itp. Jeśli potrzebujesz operować tylko na literach ASCII, możesz użyć `.downcase!` z opcją, która ogranicza działanie do 7-bitowego ASCII.

Implementacja `.downcase` wykorzystuje tablice mapowania znaków, gdzie każdemu znakowi wielko-litery odpowiada jego mało-literowa wersja. Podczas konwersji Ruby po prostu zamienia każdy znak zgodnie z tą tablicą.

## See Also (Zobacz Również)
- Dokumentacja `.downcase` [Ruby Docs: downcase](https://ruby-doc.org/core-2.7.0/String.html#method-i-downcase)
- Co nowego w Ruby? [Ruby News](https://www.ruby-lang.org/en/news/)
