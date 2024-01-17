---
title:                "Skriving av tester"
html_title:           "Elixir: Skriving av tester"
simple_title:         "Skriving av tester"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/elixir/writing-tests.md"
---

{{< edit_this_page >}}

Hva & Hvorfor?
Å skrive tester er en viktig del av programmering. Det betyr at man skriver kode som sjekker om koden vi allerede har laget fungerer som den skal. Dette er viktig for å sikre at koden fungerer som forventet og for å fange eventuelle feil før de blir et problem for brukerne.

Hvordan:
Tester skrives ved hjelp av spesielle verktøy og biblioteker som er tilgjengelige for Elixir. Disse lar deg skrive tester på en strukturert og effektiv måte. La oss se på et eksempel:

```Elixir
def test_division do
  assert 10 / 2 == 5
end
```

I dette eksemplet bruker vi "assert" for å sjekke at 10 delt på 2 gir oss 5 som svar. Hvis dette ikke stemmer, vil testen feile og gi oss beskjed om at noe er galt.

Dypdykk:
Selve konseptet med å skrive tester er ikke nytt, men det har blitt mer og mer populært i moderne programmering. Alternativene til å skrive tester inkluderer manuell testing, som er tidkrevende og ikke alltid pålitelig, og andre automatiske testrammeverk som RSpec eller JUnit.

Å skrive tester i Elixir kan også bety å ta i bruk test-drevet utvikling (TDD). Dette er en metodikk der man skriver testene før man skriver selve koden, som kan hjelpe til med å produsere bedre og mer feilfri kode.

Tester kan også være nyttige for å dokumentere koden og fungere som en slags bruksanvisning for andre utviklere som skal jobbe med koden senere.

Se også:
- Offisiell Elixir dokumentasjon: https://hexdocs.pm/exunit/ExUnit.html
- En enkel introduksjon til TDD med Elixir: https://thoughtbot.com/blog/testing-your-first-elixir-module