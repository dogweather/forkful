---
date: 2024-01-26 00:56:47.327439-07:00
description: "Jak to zrobi\u0107: Ruby u\u017Cywa `begin`, `rescue`, `ensure` i `end`\
  \ do obs\u0142ugi b\u0142\u0119d\xF3w. Zawijasz ryzykowny kod w `begin` i `end`.\
  \ Je\u015Bli wyst\u0105pi b\u0142\u0105d, w\u0142\u0105cza si\u0119\u2026"
lastmod: '2024-03-13T22:44:35.939728-06:00'
model: gpt-4-1106-preview
summary: "Ruby u\u017Cywa `begin`, `rescue`, `ensure` i `end` do obs\u0142ugi b\u0142\
  \u0119d\xF3w."
title: "Obs\u0142uga b\u0142\u0119d\xF3w"
weight: 16
---

## Jak to zrobić:
Ruby używa `begin`, `rescue`, `ensure` i `end` do obsługi błędów. Zawijasz ryzykowny kod w `begin` i `end`. Jeśli wystąpi błąd, włącza się `rescue`.

```Ruby
begin
  # Ryzykowny kod umieszczamy tutaj.
  puts 10 / 0
rescue ZeroDivisionError => e
  puts "O nie! Nie możesz tego zrobić: #{e.message}"
ensure
  puts "To zawsze się wykonuje, niezależnie od błędu czy nie."
end
```

Przykładowy wynik:
```
O nie! Nie możesz tego zrobić: divided by 0
To zawsze się wykonuje, niezależnie od błędu czy nie.
```

## Pogłębienie
Historycznie obsługa błędów w językach programowania ewoluowała znacząco, gdzie wczesne języki często posiadały proste lub nie istniejące mechanizmy. Obsługa wyjątków w Ruby jest inspirowana językami takimi jak Python i Smalltalk.

Alternatywy dla `begin-rescue` w Ruby obejmują użycie `rescue` w definicjach metody lub wykorzystanie `throw` i `catch` do niestandardowego kontrolowania przepływu, chociaż nie są one używane do typowej obsługi błędów.

Jeden interesujący szczegół: wyjątki w Ruby są obiektami (instancjami klasy `Exception` i jej potomków), więc możesz definiować własne klasy błędów i robić więcej niż tylko rejestrować błędy — możesz przenosić bogaty stan wokół programu dla bardziej solidnej obsługi błędów.

## Zobacz też
- Dokumentacja Ruby na temat wyjątków i obsługi błędów: [ruby-doc.org](https://ruby-doc.org/core-3.1.0/doc/syntax/exceptions_rdoc.html)
- Szczegółowy przewodnik po najlepszych praktykach obsługi błędów w Ruby: [thoughtbot.com](https://thoughtbot.com/blog/rescue-standarderror-not-exception)
