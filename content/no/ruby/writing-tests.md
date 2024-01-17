---
title:                "Skrive tester"
html_title:           "Ruby: Skrive tester"
simple_title:         "Skrive tester"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/ruby/writing-tests.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Skriving av tester er en praksis som innebærer å skrive kode for å teste koden din. Dette hjelper programmere å finne og fikse feil i programmet sitt før det blir lansert til brukere.

## Hvordan:
For å skrive tester i Ruby kan du bruke et testrammeverk som RSpec eller minitest. Her er et eksempel på hvordan du kan teste funksjonen `add` for å sikre at den adderer to tall riktig:

```Ruby
require 'minitest/autorun'

# Testklasse for add funksjon
class AddTest < MiniTest::Test
  def test_add
    assert_equal 4, add(2, 2)
  end
end

# Funksjon for addisjon
def add(a, b)
  a + b
end
```

Når du kjører testene vil du få en utgang som indikerer om testen var vellykket eller ikke. Hvis du endrer kode i `add` funksjonen uten å endre testen, vil testen mislykkes og indikere at det er en feil i koden din.

## Dypdykk:
Skriving av tester har vært en viktig del av programmeringspraksisen i lang tid, spesielt i testdrevet utvikling (TDD). I TDD skriver programmerere testene før de skriver selve koden, noe som hjelper dem å designe og implementere kode som er lettere å teste og mer pålitelig. Det finnes også alternative testerammeeverk og metoder, som for eksempel BDD (behavior-driven development).

Når du skriver tester i Ruby, kan du bruke ulike assert-metoder fra testrammeverket for å sammenligne forventet resultat med faktisk resultat. Du kan også bruke fixtures for å sette opp en standardisert testmiljø eller mock-objekter for å simulere eksterne avhengigheter.

## Se også:
- RSpec: https://rspec.info/
- Pete Hodgson's Testing Talks: https://github.com/peteh