---
title:                "Søke og erstatte tekst"
html_title:           "Ruby: Søke og erstatte tekst"
simple_title:         "Søke og erstatte tekst"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/ruby/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

# Hvorfor
Det er ofte nødvendig å søke og erstatte tekst når man jobber med kode, enten det er for å rette opp skrivefeil eller for å gjøre større endringer i koden. Å kunne søke og erstatte effektivt kan spare tid og forbedre kvaliteten på koden din.

## Slik gjør du det
Det er flere måter å søke og erstatte tekst i Ruby. Den mest grunnleggende måten er å bruke metoden `gsub` på en streng: 

``` Ruby
my_string = "Hei verden"
my_string.gsub("verden", "Ruby")
# Output: "Hei Ruby"
```

Du kan også bruke regulære uttrykk for å søke etter mønstre i teksten din og erstatte dem:

``` Ruby
my_string = "123abc"
my_string.gsub(/[a-z]/, "X")
# Output: "123XXX"
```

Hvis du ønsker å gjøre endringer i en fil, kan du bruke kommandolinjetolsen `sed`:

```
sed -i 's/verden/Ruby/g' hello.rb
```
Dette vil oppdatere filen `hello.rb` slik at alle forekomster av "verden" erstattes med "Ruby".

## Dypdykk
Søking og erstatting av tekst kan gjøres på en mer avansert måte ved å bruke metoden `scan` for å finne alle forekomster av et mønster og deretter bruke en blokk for å erstatte dem individuelt:

``` Ruby
my_string = "Hei verden"
my_string.scan(/e/) { |match| print match.replace("a") }
# Output: Hai vardan
```

Du kan også bruke regulære uttrykk med flagg for å få mer spesifikke søk og erstatninger. For eksempel kan flagget `/i` brukes for å ignorere store og små bokstaver:

``` Ruby
my_string = "Hallo Verden"
my_string.gsub(/verden/i, "Ruby")
# Output: "Hallo Ruby"
```

For en mer omfattende forståelse av søking og erstatting i Ruby, anbefaler vi å se på Ruby-dokumentasjonen for `String`-klassen.

## Se også
- [Ruby-dokumentasjon for `String#gsub`](https://ruby-doc.org/core/String.html#method-i-gsub)
- [Ruby-dokumentasjon for regulære uttrykk](https://ruby-doc.org/core/Regexp.html)
- [En guide til å bruke `sed`](https://www.digitalocean.com/community/tutorials/the-basics-of-using-the-sed-stream-editor-to-manipulate-text-in-linux)