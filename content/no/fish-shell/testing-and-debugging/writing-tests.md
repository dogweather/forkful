---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:30:36.842264-07:00
description: "\xC5 skrive tester i Fish Shell inneb\xE6rer \xE5 lage skript som automatisk\
  \ kj\xF8rer koden din for \xE5 validere dens oppf\xF8rsel mot forventede resultater.\
  \ Denne\u2026"
lastmod: '2024-03-11T00:14:14.834020-06:00'
model: gpt-4-0125-preview
summary: "\xC5 skrive tester i Fish Shell inneb\xE6rer \xE5 lage skript som automatisk\
  \ kj\xF8rer koden din for \xE5 validere dens oppf\xF8rsel mot forventede resultater.\
  \ Denne\u2026"
title: Skrive tester
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Å skrive tester i Fish Shell innebærer å lage skript som automatisk kjører koden din for å validere dens oppførsel mot forventede resultater. Denne praksisen er avgjørende ettersom den sikrer at shell-skriptene dine fungerer som tiltenkt, fanger opp feil tidlig og gjør vedlikehold enklere.

## Hvordan:

Fish har ikke innebygd testrammeverk som noen andre programmeringsmiljøer. Derimot, kan du skrive enkle testskript som bruker påstander for å sjekke oppførselen til funksjonene dine. I tillegg kan du utnytte tredjepartsverktøy som `fishtape` for en mer omfattende testsuite.

### Eksempel 1: Grunnleggende Testskript

La oss starte med en grunnleggende funksjon i Fish som beregner summen av to tall:

```fish
function add --description 'Legger sammen to tall'
    set -l sum (math $argv[1] + $argv[2])
    echo $sum
end
```

Du kan skrive et grunnleggende testskript for denne funksjonen slik:

```fish
function test_add
    set -l result (add 3 4)
    if test $result -eq 7
        echo "test_add bestått"
    else
        echo "test_add feilet"
    end
end

test_add
```

Å kjøre dette skriptet vil gi utdata:

```
test_add bestått
```

### Eksempel 2: Bruke Fishtape

For en mer robust testløsning, kan du bruke `fishtape`, en TAP-produserende testkjører for Fish.

Først, installer `fishtape` hvis du ikke allerede har gjort det:

```fish
fisher install jorgebucaran/fishtape
```

Deretter, opprett en testfil for din `add` funksjon, f.eks., `add_test.fish`:

```fish
test "Å legge sammen 3 og 4 gir 7"
    set result (add 3 4)
    echo "$result" | fishtape
end
```

For å kjøre testen, bruk følgende kommando:

```fish
fishtape add_test.fish
```

Eksempel på utdata kan se slik ut:

```
TAP versjon 13
# Å legge sammen 3 og 4 gir 7
ok 1 - test_add bestått
```

Dette forteller deg at testen ble bestått vellykket. `fishtape` lar deg strukturere mer detaljerte tester og gir informativ utdata, noe som letter feilsøkingen og gir omfattende testdekning for dine Fish-skript.
