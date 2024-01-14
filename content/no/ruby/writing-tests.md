---
title:                "Ruby: Skriver tester"
programming_language: "Ruby"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/ruby/writing-tests.md"
---

{{< edit_this_page >}}

## Hvorfor: Hvorfor skal vi skrive tester?

Svaret er enkelt: Skrive tester for å sikre at koden vår fungerer som den skal. Tester hjelper til med å oppdage feil og bugs i koden vår, og tillater oss å fikse dem før de blir et større problem. Det er også en god måte å dokumentere koden vår på og gjøre den mer lesbar for andre utviklere.

## Hvordan: Eksempler på testing i Ruby

La oss si at vi har en enkel funksjon som legger sammen to tall:

```Ruby
def add_numbers(x, y)
  return x + y
end
```

Vi kan skrive en enkel enhetstest for denne funksjonen ved å bruke RSpec:

```Ruby
require 'rspec/autorun'

describe "add_numbers" do 
  it "adds two numbers correctly" do 
    expect(add_numbers(3, 4)).to eq(7)
  end 
end 
```

Når vi kjører denne testen, vil vi få en grønn markering som betyr at testen har passert. Men hva skjer hvis vi endrer funksjonen vår litt:

```Ruby
def add_numbers(x, y)
  return x * y
end
```

Nå når vi kjører testen igjen, vil vi få en rød markering som betyr at testen feilet. Dette indikerer at vår endring har ført til en uventet feil, og vi må gå tilbake og fikse koden vår.

## Dykk dypere: Mer informasjon om testing

Det finnes flere forskjellige testrammeverk som kan brukes i Ruby, som RSpec, Minitest og Cucumber. Alle disse har sine egne fordeler og ulemper, men det viktigste er å velge et som passer best for ditt prosjekt og din personlige preferanse.

Det er også viktig å forstå forskjellen mellom enhetstesting og integrasjonstesting. Enhets-testing tester enkeltkomponenter av koden vår, mens integrasjonstesting tester hvordan disse komponentene fungerer sammen. Det er viktig å balansere begge typer testing for å sikre at koden vår fungerer som den skal.

## Se også

- [RailsGuides: Testing](https://guides.rubyonrails.org/testing.html)
- [Better Specs: RSpec guidelines](https://www.betterspecs.org/)
- [Minitest: Documentasjon](https://github.com/seattlerb/minitest)