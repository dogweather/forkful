---
title:                "Sammenslåing av strenger"
html_title:           "Ruby: Sammenslåing av strenger"
simple_title:         "Sammenslåing av strenger"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/ruby/concatenating-strings.md"
---

{{< edit_this_page >}}

## Hvorfor

Det å konkatenerer strenger, eller slå sammen flere strenger til en, er en viktig del av programmering i Ruby. Det lar deg bygge mer komplekse og dynamiske strenger, og er en essensiell ferdighet for ethvert Ruby-prosjekt.

## Hvordan man gjør det

Det er flere måter å konkatenerere strenger i Ruby på, og her er to enkle eksempler:

```ruby
first_name = "Sara"
last_name = "Johansen"

# Ved å bruke pluss-operatøren
full_name = first_name + " " + last_name
puts full_name # Output: Sara Johansen

# Ved å bruke syntaksen for interpolering av variabler
full_name = "#{first_name} #{last_name}"
puts full_name # Output: Sara Johansen
```

Vi kan også bruke `concat`-metoden for å konkatenerere strenger på en sikker og effektiv måte. Dette er spesielt nyttig når du jobber med store mengder tekst som kan føre til minnelekkasjer.

```
first_name = "Sara"
last_name = "Johansen"
full_name = first_name.concat(" ", last_name)
puts full_name # Output: Sara Johansen
```

## Dykk dypere

Når du konkatenerer strenger i Ruby, må du være oppmerksom på hvilken datatype du arbeider med. Strenger og tall kan ikke være direkte sammenkoblet, så du må bruke `to_s`-metoden for å konvertere tall til strenger før du kan kombinere dem. Det er også viktig å huske på at Ruby er en dynamisk språk, så variabler kan endre datatype mens programmet kjører.

Du kan også bruke `<<`-operatøren for å konkatenerere strenger, men denne endrer den første strengen direkte og returnerer den modifiserte versjonen. Dette kan føre til uventet oppførsel hvis du ikke er forsiktig.

## Se også

- [Ruby String dokumentasjon](https://ruby-doc.org/core-3.0.0/String.html)
- [Ruby Concat metode dokumentasjon](https://ruby-doc.org/core-3.0.0/String.html#method-i-2B)