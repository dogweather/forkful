---
title:                "Utskrift av feilsøkingsutdata"
html_title:           "Ruby: Utskrift av feilsøkingsutdata"
simple_title:         "Utskrift av feilsøkingsutdata"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/ruby/printing-debug-output.md"
---

{{< edit_this_page >}}

## Hvorfor

Å skrive ut debugging-utdata i Ruby er en nyttig og effektiv måte å feilsøke og forstå hva som skjer i ditt program. Det kan hjelpe deg med å identifisere og løse feil og problemer i koden din, samt forbedre din generelle forståelse av hvordan koden fungerer.

## Hvordan

For å skrive ut debugging-utdata i Ruby, kan du bruke `p` eller `puts` metoden. Disse metodene vil skrive ut variablene og deres verdier på konsollen, slik at du kan se hva som skjer i koden din.

```Ruby
# Her er et eksempel på å bruke `p` for å skrive ut en variabel:
age = 25
p age # output: 25

# Du kan også skrive ut flere variabler på en gang:
name = "Emma"
country = "Norway"
p name, country # output: "Emma" "Norway"
```

```Ruby
# Her er et eksempel på å bruke `puts` for å skrive ut en streng:
puts "Hello world!" # output: Hello world!

# Du kan også bruke `puts` for å skrive ut variabler og strenger sammen:
age = 25
puts "Jeg er #{age} år gammel." # output: Jeg er 25 år gammel.
```

## Deep Dive

Det er viktig å bruke debugging-utdata på riktig måte for å få mest mulig ut av det. Her er noen tips for å hjelpe deg med å bruke det effektivt:

- Unngå å skrive ut for mye unødvendig informasjon, da dette kan gjøre det vanskelig å finne de viktige punktene. Velg spesifikke variabler og steder i koden din der du tror et problem kan oppstå.
- Bruk `p` i stedet for `puts` når du arbeider med komplekse strukturer som arrays og hashes, da `p` vil skrive ut informasjonen på en mer detaljert måte.
- Kombiner `p` og `puts` for å få en bedre forståelse av hva som skjer. Skriv ut viktige variabler og strings for å se om de har den riktige verdien på de riktige punktene i koden.

## Se også

- [Ruby Dokumentasjon](https://www.ruby-lang.org/no/documentation/) for mer informasjon om debugging-utdata og andre nyttige Ruby-funksjoner.
- [Ruby on Rails Tutorial](https://www.railstutorial.org/) for å lære mer om Ruby og Ruby on Rails-rammeverket.