---
title:                "Bash: Skriving av tester"
programming_language: "Bash"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/bash/writing-tests.md"
---

{{< edit_this_page >}}

## Hvorfor

Å skrive tester i Bash-programmering er en viktig del av å utvikle pålitelig og feilfri kode. Tester bidrar til å identifisere og rette opp i eventuelle feil eller mangler i koden, noe som resulterer i bedre og mer stabil programvare. Det kan virke som en ekstra arbeidsbyrde, men langsiktig vil det spare deg for mye tid og frustrasjon.

## Hvordan gjøre det

Bash er et populært skriptspråk som brukes i mange Linux-distribusjoner og for administrasjon av servere. Skriving av tester i Bash utføres ved hjelp av [[Bats (Bash Automated Testing System)|https://github.com/sstephenson/bats]], et testverktøy spesielt utviklet for Bash-skript. La oss se på et eksempel på hvordan du kan skrive en test for en enkel funksjon som legger sammen to tall:

```
load 'support.bash' #definerer hjelpefunksjoner og bytter til testmappen

@test "Legg sammen to tall" {
  run sum 5 5 # kjører funksjonen sum med argumentene 5 og 5
  assert_output "10" # resultatet bør være 10
}
```
Koden over definerer en test som sjekker om funksjonen "sum" returnerer riktig resultat når den får inn to tall som argumenter. Bats vil da kjøre koden og sjekke om resultatet stemmer med forventet output. Om ikke, vil testen feile og du vil få beskjed om at det er en bug som må fikses.

## Dypdykk

Et godt skrevet testtilfelle bør være enkelt og tydelig, og bør alltid følge "Arrange, Act, Assert"-mønsteret. Dette betyr at testen bør først forberede miljøet (arrange), deretter kjøre koden som skal testes (act), og til slutt sjekke om resultatet stemmer overens med forventet output (assert).

I tillegg er det viktig å skrive flere tester for å sikre at koden fungerer i ulike situasjoner, som for eksempel når ugyldige argumenter blir gitt eller når funksjonen kalles flere ganger.

En annen nyttig funksjon i Bats er muligheten til å gruppere sammen flere tester innenfor samme "describe" blokk, noe som gjør det enklere å organisere og forstå testene.

## Se også

- [[Bats dokumentasjon|https://github.com/sstephenson/bats/blob/master/docs/01.writing-tests.md#assertions]]
- [[The Art of Command Line|https://github.com/jlevy/the-art-of-command-line#automation-and-overcoming-internal-resistance]]
- [[Bash-bibliotek med nyttige testfunksjoner|https://github.com/lehmannro/assert.sh]]