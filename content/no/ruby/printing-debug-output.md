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

## Hva & hvorfor?
Debugging er en viktig del av programmering og innebærer å finne og fikse feil i koden vår. En enkel og effektiv måte å finne disse feilene er å skrive ut informasjon om variabler, funksjoner og andre deler av koden mens programmet kjører. Dette kalles å printe debug output. Ved å gjøre dette kan vi se hva som skjer i koden vår og finne eventuelle feil som må rettes.

## Slik gjør du det:
For å printe debug output i Ruby, kan du bruke metoden "p" eller "puts". Metoden "p" vil gi deg en detaljert utskrift av variabler og objekter, mens "puts" vil gi deg en enkel utskrift. La oss se på et eksempel:

```ruby 
# Definerer en variabel 
x = 10 

# printer debug output med metoden "p" 
p x 
# Output: 10 

# printer debug output med metoden "puts" 
puts x 
# Output: 10 
```

Som du kan se, gir begge metodene samme resultat, men "p" gir oss mer informasjon om variabelen.

## Dykk ned i det:
Historisk sett har programmers brukt "puts" til å printe debug output, men "p" ble introdusert i Ruby 1.9 og har blitt mer populær på grunn av sin detaljerte utskrift. Alternativene til å printe debug output inkluderer også å bruke en logger eller debuggingsverktøy som Pry eller Byebug.

Implementeringen av "p"-metoden i Ruby er ganske enkel. Den bruker egenskapene til "inspect" metoden for å skrive ut en string-representasjon av variabelen eller objektet. Dette er nyttig for å se alle attributter og verdier til et objekt.

## Se også:
For mer informasjon om debugging i Ruby, kan du sjekke ut disse kildene:

- [Ruby Docs: Debugging](https://ruby-doc.org/core-2.7.1/doc/guides/debugging.html)
- [Pry](https://github.com/pry/pry)
- [Byebug](https://github.com/deivid-rodriguez/byebug)