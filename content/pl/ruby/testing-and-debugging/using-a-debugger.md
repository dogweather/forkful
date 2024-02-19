---
aliases:
- /pl/ruby/using-a-debugger/
date: 2024-01-26 04:09:56.989343-07:00
description: "U\u017Cywanie debugera w Ruby daje programistom supermoc, umo\u017C\
  liwiaj\u0105c\u0105 zatrzymanie ich kodu, inspekcj\u0119 zmiennych oraz krok po\
  \ kroku przegl\u0105danie ich kodu.\u2026"
lastmod: 2024-02-18 23:08:50.130053
model: gpt-4-0125-preview
summary: "U\u017Cywanie debugera w Ruby daje programistom supermoc, umo\u017Cliwiaj\u0105\
  c\u0105 zatrzymanie ich kodu, inspekcj\u0119 zmiennych oraz krok po kroku przegl\u0105\
  danie ich kodu.\u2026"
title: Korzystanie z debugera
---

{{< edit_this_page >}}

## Co i Dlaczego?

Używanie debugera w Ruby daje programistom supermoc, umożliwiającą zatrzymanie ich kodu, inspekcję zmiennych oraz krok po kroku przeglądanie ich kodu. Ludzie robią to, aby wyeliminować błędy, zrozumieć przepływ kodu oraz, aby zobaczyć dokładnie, co ich zaklęcia (kod) robią, kiedy magia działa — albo i nie.

## Jak to zrobić:

Ruby jest wyposażony w wbudowany debugger o nazwie `byebug`. Najpierw, dołącz `byebug` do swojego Gemfile i uruchom `bundle install`. Następnie, umieść `byebug` dokładnie tam, gdzie chcesz, aby Twój program zrobił przerwę.

```Ruby
require 'byebug'

def calculate_magic(number)
  byebug
  magic_number = number * 7
  return magic_number
end

puts calculate_magic(6)
```

Uruchomienie tego skryptu zatrzyma wykonanie na `byebug`, i zostaniesz przeniesiony do interaktywnej sesji, gdzie możesz wpisywać komendy takie jak:

```
step
next
continue
var local
```

Przykładowy wynik da Ci monit wyglądający tak:

```
[2, 11] in example.rb
    2: 
    3: def calculate_magic(number)
    4:   byebug
=>  5:   magic_number = number * 7
    6:   return magic_number
    7: end
    8: 
    9: puts calculate_magic(6)
(byebug) 
```

## Dogłębna analiza:

Dawno temu, przed `byebug`, Rubyści używali `debugger` i `pry`. Ten drugi, `pry`, jest czymś więcej niż debuggerem; to potężne REPL, które również może być używane do debugowania z punktem przerwania `binding.pry`.

Alternatywy dla `byebug` w Ruby to między innymi `pry-byebug`, który łączy funkcjonalność `pry` z `byebug`, oraz `ruby-debug`, który jest starszym gemem nieutrzymywanym aktywnie.

Kiedy wywołasz `byebug`, debugger zawiesza wykonanie Twojego kodu i daje Ci wgląd w czasie wykonania. Możesz zobaczyć i zmienić zmienne, przeskoczyć do różnych miejsc w kodzie, a nawet uruchomić niektóre linie kodu Ruby krok po kroku. To trochę jak posiadanie zdolności podróżowania w czasie dla Twojego kodu Ruby.

## Zobacz także:

- Repozytorium Byebug na GitHubie: [https://github.com/deivid-rodriguez/byebug](https://github.com/deivid-rodriguez/byebug)
- Dokumentacja Pry: [https://github.com/pry/pry](https://github.com/pry/pry)
- Przewodnik po debugowaniu aplikacji Rails: [https://guides.rubyonrails.org/debugging_rails_applications.html](https://guides.rubyonrails.org/debugging_rails_applications.html)
