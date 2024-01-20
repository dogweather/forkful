---
title:                "Wykorzystanie wyrażeń regularnych"
html_title:           "Arduino: Wykorzystanie wyrażeń regularnych"
simple_title:         "Wykorzystanie wyrażeń regularnych"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/ruby/using-regular-expressions.md"
---

{{< edit_this_page >}}

## What & Why?
W Ruby regexy to narzędzia do wyszukiwania wzorców w tekście. Służą do walidacji, przeszukiwania czy też transformowania stringów – zwiększają elastyczność i efektywność kodu.

## How to:
Użycie regexpa w praktyce:

```Ruby
# Wyszukiwanie słowa 'ruby'
text = "Lubię programować w Ruby!"
pattern = /ruby/i
puts "Znaleziono!" if text.match(pattern)

# Output: Znaleziono!

# Podmiana tekstu
text = "Jest zimno i pada śnieg"
new_text = text.gsub(/zimno/, 'ciepło')
puts new_text

# Output: Jest ciepło i pada śnieg

# Capturing groups - wyłuskanie danych
date = "Data wydarzenia: 2023-04-26"
pattern = /(\d{4})-(\d{2})-(\d{2})/
if date =~ pattern
  puts "Rok: #{$1}, Miesiąc: #{$2}, Dzień: #{$3}"
end

# Output: Rok: 2023, Miesiąc: 04, Dzień: 26
```

## Deep Dive:
Regularne wyrażenia, czyli regexy, pojawiły się w latach 50. XX wieku. Są standardem w większości języków programowania. Alternatywą dla regexów jest manualne przeszukiwanie stringów, co jest pracochłonne i mniej wydajne. W Ruby regexy są wbudowane i korzystają z różnych implementacji, np. Oniguruma.

## See Also:
- Dokładniejsze wyjaśnienie regexów: [rubular.com](http://rubular.com/)
- Interaktywne tutoriali: [regexone.com](https://regexone.com/)