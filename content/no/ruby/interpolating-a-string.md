---
title:                "Interpolering av en streng"
html_title:           "Bash: Interpolering av en streng"
simple_title:         "Interpolering av en streng"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/ruby/interpolating-a-string.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Interpolering av en streng er det å sette inn variabler eller uttrykk inn i en streng i koden. Vi gjør dette for å lage dynamiske strenger som kan endres basert på forskjellige variabler eller tilstander i koden.

## Hvordan
Her er et enkelt eksempel på hvordan du kan interpolere en streng i Ruby:

```Ruby
navn = "Ola"
hilsen = "Hei, #{navn}!"
puts hilsen
```

Dette vil skrive ut:

```
Hei, Ola!
```

Merk at vi bruker `#{...}` inne i strengen for å interpolere variabelen `navn` i strengen.

## Dypdykk
String-interpolering ble introdusert i Ruby i versjon 1.9. Før dette måtte programmerere bruke pluss-operatøren eller `concat`-metoden for å sette sammen strenger, som kunne være rotete og ineffektivt.

Alternativer til strenginterpolering inkluderer bruk av `sprintf` metode eller `%` operatør, men disse er generelt sett mer kompliserte å bruke og lese.

En detalj om implementering av strenginterpolering i Ruby: Ruby tolker strengen og erstatter interpolasjoner med verdien av uttrykket i interpolasjonen ved kjøring, ikke ved tolking.

## Se Også
1. [The Ruby Programming Language](https://www.ruby-lang.org/en/documentation/) - Offisiell dokumentasjon for Ruby 
2. [Ruby Inside](https://www.rubyinside.com/) - Populært online Ruby-magasin
3. [String Interpolation in Ruby](https://www.rubyguides.com/2018/11/ruby-string-formatting/) - En mer dyptgående guide til string-interpolering i Ruby.