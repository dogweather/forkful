---
title:                "Å skrive tester"
html_title:           "Ruby: Å skrive tester"
simple_title:         "Å skrive tester"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/ruby/writing-tests.md"
---

{{< edit_this_page >}}

## Hvorfor

Å skrive tester i programmering er en viktig praksis for å sikre at koden vår fungerer riktig og unngå eventuelle feil og bugs. Det sparer oss også mye tid og frustrasjon i det lange løp ved å hjelpe oss med å finne feil i koden raskt.

## Hvordan gjøre det

Å skrive tester kan virke skremmende i begynnelsen, men med litt praksis blir det en rutine som ikke tar mye tid. For å lage en test i Ruby, må vi følge disse enkle trinnene:

1. Opprett en testfil ved å legge til `_test.rb` i navnet på filen din, for eksempel `calculator_test.rb`. Dette vil fortelle Ruby at det er en testfil.
2. I testfilen vår, bruker vi `require` for å importere filen vi vil teste, for eksempel `require_relative "calculator"`.
3. Opprett en testfunksjon ved hjelp av `def` nøkkelordet og gi den et beskrivende navn, for eksempel `test_addition`.
4. Inne i testfunksjonen vår, bruker vi `assert_equal` metoden for å sjekke om den faktiske verdien er lik den forventede verdien.
5. Kjør testen ved å bruke kommandoen `ruby filnavn_test.rb` i konsollen. Hvis testen ikke passerer, vil den gi en feilmelding og hjelpe deg med å finne feilen din.

Et eksempel på hvordan dette kan se ut i praksis:

```Ruby
require_relative "calculator"

def test_addition
  result = add(2, 3) # Vi kaller på funksjonen vi vil teste
  assert_equal(5, result) # Vi sjekker om den faktiske verdien er lik 5, som er den forventede verdien
end

def test_subtraction
  result = subtract(5, 3)
  assert_equal(2, result)
end

def test_multiplication
  result = multiply(4, 3)
  assert_equal(12, result)
end

def test_division
  result = divide(10, 2)
  assert_equal(5, result)
end
```

Hvis vi kjører dette, vil vi få en melding som sier at alle testene er bestått. Hvis vi endrer en funksjon og kjører testen på nytt, vil vi få en feilmelding og vite akkurat hvor i koden det er noe galt.

## Dypere dykk

Det er mange måter å skrive tester på i Ruby, og vi kan bruke ulike metoder som `assert_equal`, `assert_true`, og `assert_includes` for å sjekke ulike forhold i koden vår. Vi kan også organisere testene våre i forskjellige testklasser for å gjøre koden vår mer lesbar og håndtere ulike scenarioer.

I tillegg kan vi bruke diverse testing gems, som RSpec og MiniTest, for å gjøre testene våre enda mer robuste og fleksible. Disse gems gir oss også muligheten til å skrive testene våre i et mer "naturlig språk", noe som kan gjøre det enklere å forstå koden for andre utviklere.

## Se også

- [Boken "The Minitest Cookbook"](https://chriskottom.com/minitestcookbook/) av Chris Kottom
- [Video av Kent Beck om testdrevet utvikling i Ruby](https://www.youtube.com/watch?v=J4dlF0kcThQ)
- [RSpec testing gem](https://rspec.info/) for mer avansert testing i Ruby