---
title:                "Ruby: Utskrift av feilsøkingsdata"
programming_language: "Ruby"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/ruby/printing-debug-output.md"
---

{{< edit_this_page >}}

# Hvorfor

Å skrive kode kan være en frustrerende opplevelse, spesielt når ting ikke fungerer som de skal. Det kan virke som om du har prøvd alt, men likevel ikke finner ut hva som er galt. Det er her debugging kommer inn i bildet. En enkel og effektiv måte å finne feil i koden din er å bruke print-utsagn for å se hva som skjer under kjøretid. Dette kan være en livredder når du forsøker å løse komplekse problemer eller forstå hvordan koden din fungerer.

# Slik gjør du det

Skrive ut debug-output i Ruby er enkelt og kan gjøres på noen få forskjellige måter. En av de mest populære metodene er å bruke metoden `puts`, som står for "put string". Dette gjør at du kan skrive ut en hvilken som helst variabel eller streng i konsollen mens programmet ditt kjører. For eksempel:

```Ruby
puts "dette er en test"
# => dette er en test
```

Du kan også inkludere variabler i `puts`-uttrykket ved å bruke interpolering. Dette betyr at du kan legge til verdien av en variabel i en streng. For eksempel:

```Ruby
nummer = 5
puts "tallet er #{nummer}"
# => tallet er 5
```

Du kan også bruke `p`, som står for "print", for å skrive ut en variabel, men også for å se dens type og struktur. Dette kan være spesielt nyttig når du jobber med komplekse objekter eller arrays. For eksempel:

```Ruby
array = [1, 2, 3]
p array
# => [1, 2, 3]
```

# Dykk dypere

Det er en rekke andre utskriftsmetoder som kan være nyttige når du debugger din Ruby-kode. For eksempel har vi `print`, som bare skriver ut teksten og ikke legger til en ny linje etter. Dette kan være nyttig hvis du ønsker å skrive ut flere ting på samme linje. Det er også `pp`, som står for "pretty print", og gjør at utdataen ser mer strukturert og lesbar ut. Dette kan være spesielt nyttig for å få et bedre overblikk over komplekse objekter.

Det er også verdt å merke seg at det kan være lurt å bruke en betingelse når du skriver ut debugging-uttrykk. For eksempel kan du legge til en `if`-setning for å kun skrive ut variabler hvis en bestemt betingelse er oppfylt. På denne måten kan du unngå å oversvømme konsollen med unødvendig informasjon.

# Se også

* [Guide til debugging i Ruby](https://www.sitepoint.com/ruby-debugging/)
* [10 tips for effektiv debugging i Ruby](https://www.sitepoint.com/tips-effective-debugging-ruby/)
* [Offisiell dokumentasjon for utskriftsmetoder i Ruby](https://ruby-doc.org/core-2.7.0/doc/syntax/control_expressions_rdoc.html#label-Print)