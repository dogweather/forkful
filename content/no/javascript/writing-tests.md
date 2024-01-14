---
title:                "Javascript: Skrive tester"
programming_language: "Javascript"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/javascript/writing-tests.md"
---

{{< edit_this_page >}}

## Hvorfor

De aller fleste som har drevet med programmering har nok hørt om testing, og kanskje til og med skrevet noen tester selv. Men hvorfor er det egentlig viktig å skrive tester? Er det ikke bare en ekstra byrde og en tidkrevende oppgave? Vel, svaret er enkelt: tester hjelper oss med å forbedre kvaliteten på koden vår. Ved å skrive tester kan vi oppdage eventuelle feil og feilmarginer i koden vår tidlig, og dermed unngå å måtte fikse dem senere når de kan være mye vanskeligere å fikse. Det kan også hjelpe oss med å sikre at koden vår fungerer som den skal, og gjør det enklere å vedlikeholde koden på lang sikt. 

## Hvordan

Å skrive tester i Javascript er enkelt og kan gjøres med et par ulike verktøy. Det finnes flere populære rammeverk som Jest, Mocha og Chai som kan brukes til å skrive tester. I eksemplet under vil jeg vise hvordan man kan skrive en enkel test med Jest: 

```Javascript 
const multiply = (x, y) => {
  return x * y;
}

test("Multiplisere to tall", () => {
  expect(multiply(2,3)).toBe(6);
})
```

I dette eksempelet definerer vi en funksjon som multipliserer to tall, og så lager vi en test som sjekker at funksjonen faktisk returnerer riktig verdi. For å kjøre testen kan vi enten bruke Jest sitt innebygde verktøy, eller integrere det i vår utviklingsprosess. Når testen kjører, vil vi se at den passerer siden funksjonen returnerer riktig verdi, men om vi endrer på funksjonen slik at den for eksempel returnerer summen i stedet, vil testen feile og fortelle oss at det er noe galt med funskjonen vår. Dette er bare et enkelt eksempel på hvordan man kan skrive tester, men det finnes mange andre muligheter og ulike bruksområder.

## Dypdykk

Selv om det å skrive tester kan virke som en enkel oppgave, kan det være lurt å dykke litt dypere ned i emnet for å få fullt utbytte av å skrive tester. Når man skriver tester er det viktig å tenke på hva som bør testes og hvordan man tester det. Det finnes en rekke ulike typer tester som kan være relevant, som for eksempel enhetstester, integrasjonstester og end-to-end tester. Alle disse tester ulike deler av koden vår og bidrar til å oppdage feil og forbedre kvaliteten på koden.

Det er også viktig å tenke på hva slags input koden vår vil motta, og om vi håndterer dette på riktig måte. Ved å teste ulike scenarioer med forskjellige typer input, kan vi sikre at koden vår vil fungere som forventet i alle tilfeller. Det kan også være lurt å skrive tester før man begynner å kode, for å sørge for at man har en klar forståelse av hva koden skal gjøre og hvordan den skal fungere.

## Se også

- [Jest documentation](https://jestjs.io/docs/getting-started)
- [Mocha documentation](https://mochajs.org/)
- [Chai documentation](https://www.chaijs.com/)