---
date: 2024-01-26 00:56:47.327439-07:00
description: "Obs\u0142uga b\u0142\u0119d\xF3w polega na oczekiwaniu nieoczekiwanego\
  \ w kodzie \u2014 zarz\u0105dzaniu pomy\u0142kami i problemami w spos\xF3b \u0142\
  agodny, bez awarii. Programi\u015Bci robi\u0105 to, by\u2026"
lastmod: '2024-03-13T22:44:35.939728-06:00'
model: gpt-4-1106-preview
summary: "Obs\u0142uga b\u0142\u0119d\xF3w polega na oczekiwaniu nieoczekiwanego w\
  \ kodzie \u2014 zarz\u0105dzaniu pomy\u0142kami i problemami w spos\xF3b \u0142\
  agodny, bez awarii. Programi\u015Bci robi\u0105 to, by\u2026"
title: "Obs\u0142uga b\u0142\u0119d\xF3w"
weight: 16
---

## Co i Dlaczego?

Obsługa błędów polega na oczekiwaniu nieoczekiwanego w kodzie — zarządzaniu pomyłkami i problemami w sposób łagodny, bez awarii. Programiści robią to, by kontrolować przepływ, gdy coś pójdzie źle oraz by zapewnić płynne doświadczenie użytkownika.

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
