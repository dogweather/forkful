---
title:                "Å skrive tester"
html_title:           "Gleam: Å skrive tester"
simple_title:         "Å skrive tester"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/gleam/writing-tests.md"
---

{{< edit_this_page >}}

==============

## Hva & Hvorfor?

Testing i programmering er en måte å sjekke om koden vår fungerer som den skal. Det er en viktig del av utviklingsprosessen fordi det hjelper oss med å oppdage og fikse feil før de når brukerne våre.

## Slik gjør du det:

Gleam kommer med et innebygd bibliotek som heter `gleam/test` for å hjelpe oss med å skrive tester. La oss se på et enkelt eksempel:

```Gleam
import gleam/test

pub fn add(a: Int, b: Int) -> Int {
  a + b
}

test "add should add two numbers" {
  assertion expect(add(2,3)) to_equal(5)
}
```

Her importerer vi test-biblioteket og bruker `test`-funksjonen til å opprette en ny test. Inne i testen kaller vi `expect`-funksjonen for å sjekke om `add`-funksjonen gir riktig resultat når vi gir den to tall. Hvis testen feiler, vil vi få en feilmelding med informasjon om hvilken forventet og faktisk verdi som ikke stemmer.

For å kjøre testene våre, kan vi bruke `gleam test`-kommandoen i terminalen. Da vil Gleam kjøre alle testene våre og gi oss en oversikt over hvilke som har passert og eventuelle feil som ble funnet.

## Dypdykk:

Det å skrive tester er en viktig del av testdrevet utvikling (TDD), en utviklingsmetode hvor man først skriver tester og så koden som skal passe disse testene. Dette hjelper oss med å skrive kode som er mer pålitelig og enklere å vedlikeholde.

Et alternativ til å bruke Gleams innebygde test-bibliotek er å bruke et eksternt bibliotek som heter `gleam-expect`. Dette biblioteket tilbyr flere assert-funksjoner og har også støtte for å kjede flere tester sammen.

Hvis du er interessert i å se hvordan `gleam/test`-biblioteket er implementert, kan du ta en titt på kildene på GitHub: https://github.com/gleam-lang/gleam/blob/master/gleam_stdlib/src/test.gleam

## Se også:

- Gleam sin offisielle dokumentasjon om testing: https://gleam.run/book/testing
- En introduksjon til TDD med Gleam: https://gleam.run/articles/test-driven-development
- Eksempelprosjekter på GitHub som bruker test-biblioteket: https://github.com/topics/gleam-test