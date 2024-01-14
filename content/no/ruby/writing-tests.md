---
title:                "Ruby: Skriving av tester"
simple_title:         "Skriving av tester"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/ruby/writing-tests.md"
---

{{< edit_this_page >}}

## Hvorfor

Å skrive tester er en viktig del av å være en effektiv Ruby-programmerer. Det hjelper deg med å oppdage og fikse feil tidlig i utviklingsprosessen, og sikrer at koden din fungerer som forventet. 

## Hvordan

For å skrive effektive tester i Ruby, må du bruke et testrammeverk som for eksempel RSpec eller MiniTest. Disse verktøyene gir struktur og organisering til testene dine. La oss se på et eksempel på en enkel test som bruker RSpec:

```Ruby
require 'rspec'

def add(a, b)
  a + b
end

describe '#add' do
  it 'should return the sum of two numbers' do
    expect(add(2,3)).to eq(5)
  end
end
```

I dette eksempelet definerer vi en `add` metode som tar inn to tall og returnerer summen av dem. Deretter bruker vi `RSpec` til å beskrive hva metoden skal gjøre, og bruker `expect` metoden til å sjekke om resultatet er riktig.

Når vi kjører denne testen, vil vi få følgende ut:

```
1 example, 0 failures
```

Hvis vi for eksempel endrer koden vår til å returnere summen av tre tall i stedet for to, vil testen mislykkes og vi vil få en feilmelding som hjelper oss med å finne feilen og fikse den.

## Dypdykk

Å skrive gode tester handler ikke bare om å sjekke om koden fungerer som forventet. Det handler også om å skrive tester som er enkle å vedlikeholde og som dekker alle deler av koden din.

Det er viktig å skrive såkalte "assertion messages" som forklarer hva testen forventer og hva resultatet faktisk er. Dette gjør det enklere å feilsøke og fikse eventuelle feil.

Et annet viktig aspekt ved å skrive tester er å følge "Arrange, Act, Assert" mønsteret. Dette innebærer å først sette opp testen, deretter utføre handlingen du vil teste, og til slutt sjekke om resultatet er som forventet.

## Se også

- [RSpec dokumentasjon](https://relishapp.com/rspec)
- [MiniTest dokumentasjon](https://github.com/seattlerb/minitest)
- [The Art of Unit Testing](https://www.amazon.com/Art-Unit-Testing-Examples-Net/dp/1617290890) (bok)