---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:18:23.837602-07:00
description: "Jak to zrobi\u0107: Aby dopasowa\u0107 ci\u0105g znak\xF3w do prostego\
  \ wzorca, mo\u017Cesz u\u017Cy\u0107 metody `match`. Poni\u017Cej sprawdzamy, czy\
  \ s\u0142owo \"Ruby\" wyst\u0119puje w danym ci\u0105gu."
lastmod: '2024-03-13T22:44:35.920814-06:00'
model: gpt-4-0125-preview
summary: "Aby dopasowa\u0107 ci\u0105g znak\xF3w do prostego wzorca, mo\u017Cesz u\u017C\
  y\u0107 metody `match`."
title: "Korzystanie z wyra\u017Ce\u0144 regularnych"
weight: 11
---

## Jak to zrobić:


### Podstawowe dopasowywanie
Aby dopasować ciąg znaków do prostego wzorca, możesz użyć metody `match`. Poniżej sprawdzamy, czy słowo "Ruby" występuje w danym ciągu.

```ruby
if /Ruby/.match("Hello, Ruby!")
  puts "Znaleziono dopasowanie!"
end
# Output: Znaleziono dopasowanie!
```

### Dopasowywanie wzorców z użyciem zmiennych
Możesz interpolować zmienne do swojego wyrażenia regularnego za pomocą składni `#{}`, czyniąc swoje wzorce dynamicznymi.

```ruby
language = "Ruby"
if /#{language}/.match("Programowanie w Ruby jest zabawne.")
  puts "Mowa o Ruby!"
end
# Output: Mowa o Ruby!
```

### Użycie wyrażeń regularnych do zamiany
Metoda `gsub` pozwala zastąpić każde wystąpienie wzorca określonym ciągiem zastępczym.

```ruby
puts "foobarfoo".gsub(/foo/, "bar")
# Output: barbarbar
```

### Przechwytywanie
Nawiasy w wyrażeniu regularnym służą do przechwytywania części dopasowania. Metoda `match` zwraca obiekt `MatchData`, który można użyć do dostępu do przechwyceń.

```ruby
match_data = /(\w+): (\d+)/.match("Wiek: 30")
puts match_data[1] # Przechwycony etykieta
puts match_data[2] # Przechwycona wartość
# Output:
# Wiek
# 30
```

### Korzystanie z bibliotek firm trzecich
Chociaż standardowa biblioteka Ruby jest potężna, czasami możesz potrzebować bardziej wyspecjalizowanej funkcjonalności. Jednym z popularnych gemów do pracy z regex jest `Oniguruma`, który oferuje dodatkowe funkcje regex poza wbudowanym silnikiem regex Ruby.

Zainstaluj go używając:
```bash
gem install oniguruma
```

Przykład użycia może wyglądać tak (zakładając, że masz zainstalowany i zażądzony `oniguruma`):

```ruby
# To jest bardziej zaawansowany przykład i może wymagać dodatkowej konfiguracji
require 'oniguruma'

pattern = Oniguruma::ORegexp.new('(\d+)')
match_data = pattern.match("Numer to 42.")
puts match_data[1]
# Output: 42
```

Pamiętaj, że choć wyrażenia regularne są potężne, mogą stać się skomplikowane i trudne do zarządzania dla bardziej złożonych wzorców. Dąż do czytelności i rozważ alternatywne metody, jeśli twoje wyrażenie regularne stanie się zbyt zagmatwane.
