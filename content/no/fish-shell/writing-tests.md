---
title:    "Fish Shell: Skrive tester"
keywords: ["Fish Shell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/fish-shell/writing-tests.md"
---

{{< edit_this_page >}}

## Hvorfor

Å skrive tester i programmering kan virke tidkrevende og unødvendig for mange, men det er faktisk en viktig del av å utvikle høykvalitetsprogrammer. Tester sikrer at koden fungerer som den skal, og gjør det enklere å finne og fikse feil.

## Slik gjør du det

For å skrive tester i Fish Shell, må du først importere testrammen ved å legge til følgende linje i toppen av filen din:

```Fish Shell
source ~/.config/fish/fish_test.fish
```

Deretter kan du definere en test ved å bruke funksjonen "describe", etterfulgt av en beskrivelse av testen og en blokk med kode som skal testes:

```Fish Shell
describe "Addisjon"
    if [ (add 2 2) = 4 ]
        pass
    else
        fail "'add 2 2' should equal 4"
    end
end
```

For å kjøre testen, kan du bruke kommandoen "fish_test":

```Fish Shell
fish_test
```

Du vil se en oversikt over alle tester som ble kjørt, og om de ble bestått eller ikke.

## Dypdykk

Når du skriver tester, er det viktig å tenke på ulike scenarioer som kan føre til feil i koden din. Det kan også være lurt å bruke funksjoner som "setup" og "teardown" for å sette opp og rydde opp etter tester, slik at de ikke påvirker hverandre.

Du kan også bruke eksterne tester ved å legge de i separate filer og importere dem i hovedtesten din ved å bruke funksjonen "source":

```Fish Shell
source ~/tests/addition_test.fish
```

Dette gjør det enklere å organisere og vedlikeholde tester, spesielt for større prosjekter.

## Se også

- [Fish Shell sin offisielle dokumentasjon om testing](https://fishshell.com/docs/current/tutorial.html#tut_testing)
- [En guide til hvordan man skriver gode tester i Fish Shell (på engelsk)](https://medium.com/@seththompson/testing-in-fish-shell-6554cde79216)