---
title:                "Å bruke regulære uttrykk"
html_title:           "Ruby: Å bruke regulære uttrykk"
simple_title:         "Å bruke regulære uttrykk"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/ruby/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Hvorfor

Hvorfor bry seg om regulære uttrykk (regular expressions)? Regulære uttrykk er et utrolig kraftig verktøy som gjør det enkelt å finne og manipulere tekst i kode. Dette kan være spesielt nyttig når du prøver å søke etter et bestemt mønster eller verv i en streng.

## Hvordan

```Ruby
# Eksempel på bruk av regulære uttrykk
string = "Jeg elsker å kode i Ruby!"

# Søk etter en streng som inneholder ordet "elsker"
puts string.match(/elsker/)
# Output => "elsker"

# Søk etter et ord som starter med "kode"
puts string.match(/\bkode\w*/i)
# Output => "kode"

# Erstatt en streng med et annet ord
puts string.gsub(/elsker/, "hater")
# Output => "Jeg hater å kode i Ruby!"
```

Som du kan se i eksemplene over, bruker vi `/` for å definere slutten og begynnelsen av vårt regulære uttrykk. Deretter bruker vi spesielle symboler og bokstaver til å definere vårt søkemønster. `match` vil returnere den første forekomsten, mens `gsub` vil erstatte alle forekomster.

## Dypdykk

For de som ønsker å dykke dypere inn i regulære uttrykk, er det mange forskjellige symboler og koder som kan brukes for å utføre avanserte søk. Noen av de vanligste inkluderer:

- `.` brukes til å matche hvilken som helst karakter
- `*` brukes til å matche en forekomst av en karakter null eller flere ganger
- `+` brukes til å matche en forekomst av en karakter en eller flere ganger
- `?` brukes til å matche en forekomst av en karakter null eller én gang
- `()` brukes til å gruppere og ekstrahere deler av et søkemønster
- `|` brukes til å vise alternativer innen et søkemønster (for eksempel `abc|def` vil matche enten "abc" eller "def")

Det finnes også en rekke korte koder for vanlige typer av karakterer, for eksempel `\d` for tall, `\w` for bokstaver og tall, og `\s` for mellomrom og andre "tomme" karakterer.

Det er viktig å merke seg at regulære uttrykk kan bli komplekse og vanskelige å lese, spesielt for nybegynnere. Men med litt øvelse og tålmodighet, kan de bli et uvurderlig verktøy for tekstbehandling i koden din.

## Se Også

- [Ruby dokumentrasjon for regulære uttrykk](https://ruby-doc.org/core-3.0.0/Regexp.html)
- [Rubular](https://rubular.com/): et nyttig verktøy for å teste og øve på regulære uttrykk i Ruby