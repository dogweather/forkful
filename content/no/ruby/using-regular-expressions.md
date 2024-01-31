---
title:                "Bruk av regulære uttrykk"
date:                  2024-01-19
html_title:           "Bash: Bruk av regulære uttrykk"
simple_title:         "Bruk av regulære uttrykk"

category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/ruby/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Regulære uttrykk er tekst-søkemønstre. Programmerere bruker dem for å finne, hente ut, erstatte eller validere tekst.

## How to:
```Ruby
# Finne tall i en streng
streng = "Det er 12 epler og 7 bananer."
tall = streng.scan(/\d+/)
puts tall
# Output: 12 7

# Validering av e-postformat
epost = "ola@nordmann.no"
valid = /\A[\w+\-.]+@[a-z\d\-]+(\.[a-z]+)*\.[a-z]+\z/i.match?(epost)
puts valid ? "Gyldig" : "Ugyldig"
# Output: Gyldig

# Erstatte bokstaver
tekst = "foobar"
ny_tekst = tekst.sub(/o/, "0")
puts ny_tekst
# Output: f0obar
```

## Deep Dive
Regular expressions, eller regex, ble skapt på 1950-tallet. I Ruby, er Oniguruma biblioteket brukt for regex - raskt og kraftig. Alternativer? Strengmanipulering med Ruby metoder som `include?`, `split`, `gsub`, men de er mindre fleksible.

## See Also
- [Ruby-Dokumentasjon for Regulære Uttrykk](https://ruby-doc.org/core-2.7.0/Regexp.html)
- [Rubular: en Ruby-basert regulær uttrykk redigerer](http://rubular.com/)
