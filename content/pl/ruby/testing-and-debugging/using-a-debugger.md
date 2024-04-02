---
date: 2024-01-26 04:09:56.989343-07:00
description: "Ruby jest wyposa\u017Cony w wbudowany debugger o nazwie `byebug`. Najpierw,\
  \ do\u0142\u0105cz `byebug` do swojego Gemfile i uruchom `bundle install`. Nast\u0119\
  pnie, umie\u015B\u0107\u2026"
lastmod: '2024-03-13T22:44:35.936868-06:00'
model: gpt-4-0125-preview
summary: "Ruby jest wyposa\u017Cony w wbudowany debugger o nazwie `byebug`. Najpierw,\
  \ do\u0142\u0105cz `byebug` do swojego Gemfile i uruchom `bundle install`. Nast\u0119\
  pnie, umie\u015B\u0107\u2026"
title: Korzystanie z debugera
weight: 35
---

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
