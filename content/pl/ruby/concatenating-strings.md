---
title:                "Łączenie łańcuchów znaków"
date:                  2024-01-20T17:35:25.859561-07:00
model:                 gpt-4-1106-preview
simple_title:         "Łączenie łańcuchów znaków"

category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/ruby/concatenating-strings.md"
---

{{< edit_this_page >}}

## Co i dlaczego?
Konkatenacja stringów to po prostu łączenie tekstów w jeden ciąg. Programiści robią to, by składać wiadomości, generować unikatowe identyfikatory, czy też po prostu łączyć wartości z różnych źródeł.

## Jak to zrobić:
```Ruby
# Operator plus (+) do łączenia stringów
powitanie = "Cześć, " + "jak " + "się masz?"
puts powitanie
#=> Cześć, jak się masz?

# Interpolacja stringów z użyciem #{}
imie = "Ania"
powitanie = "Hej, #{imie}!"
puts powitanie
#=> Hej, Ania!

# Metoda concat
tekst = "Warszawa"
tekst.concat(" to", " moje", " miasto")
puts tekst
#=> Warszawa to moje miasto

# Operator shovel (<<), nazywany też operatorem łopatki
powitanie = "Witaj"
powitanie << " w " << "świecie Ruby!"
puts powitanie
#=> Witaj w świecie Ruby!
```

## Wgłębiamy się
Konkatenacja stringów nie zawsze była tak wydajna jak obecnie. Historycznie, każde użycie `+` tworzyło nowy obiekt w pamięci, co zużywało zasoby przy dużych lub wielokrotnych konkatenacjach. Ruby 1.9 wprowadziło zmiany w tym zakresie, optymalizując działanie. Alternatywą do `+` jest używanie metod `concat` lub `<<`, które zmieniają obiekt bezpośrednio, nie tworząc niepotrzebnie nowych. W wypadku dużych ilości danych może to znacząco wpłynąć na wydajność. Interpolacja jest zwykle najszybsza i najbardziej polecana, zwłaszcza gdy pracujemy z symbolami oraz zmiennymi.

## Zobacz również
- Ruby Documentation on String: https://ruby-doc.org/core/String.html
- "Effective Ruby" by Peter J. Jones – zawiera rozdział o przetwarzaniu stringów.
- Ruby Style Guide: https://rubystyle.guide/#string-interpolation - omawia konwencje związane z stylami łączenia stringów.
