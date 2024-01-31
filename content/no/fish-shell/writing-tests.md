---
title:                "Skriving av tester"
date:                  2024-01-19
html_title:           "Arduino: Skriving av tester"
simple_title:         "Skriving av tester"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/fish-shell/writing-tests.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Skrivetesting er en prosess for å sjekke at kode gjør det den skal. Programmerere tester for å unngå feil og sikre kvalitet.

## Hvordan gjøre det:
For å kjøre tester i Fish Shell, bruk funksjoner og `test`-kommandoen. Legg tester i filer og kjør dem manuelt eller automatisert.

```Fish Shell
function test_addition
    set result (math 2+2)
    if test $result -eq 4
        echo "Test passed: 2+2 is $result"
    else
        echo "Test failed: 2+2 is not 4"
    end
end

test_addition
```

Output:
```
Test passed: 2+2 is 4
```

## Dypdykk
Tester i Fish Shell er mindre vanlig enn i andre språk. Men, med enkle funksjoner og `test`-kommandoen er det mulig. Noen bruker rammeverk som Fishtape for struktur. Historisk sett har shell-scripting vært mindre fokusert på testing, men praksisen vinner terreng for å sikre stabil kode i automatiserte miljøer.

## Se Også
- [Fishtape](https://github.com/jorgebucaran/fishtape): Et testrammeverk spesielt laget for Fish Shell.
- [Fish Shell dokumentasjon](https://fishshell.com/docs/current/index.html): Offisiell dokumentasjon for Fish Shell.
- [Math i Fish](https://fishshell.com/docs/current/cmds/math.html): Dokumentasjon for `math`-kommandoen.
- [Test i Fish](https://fishshell.com/docs/current/cmds/test.html): Dokumentasjon for `test`-kommandoen.
