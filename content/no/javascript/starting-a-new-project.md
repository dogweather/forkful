---
title:                "Javascript: Å starte et nytt prosjekt."
programming_language: "Javascript"
category:             "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/javascript/starting-a-new-project.md"
---

{{< edit_this_page >}}

## Hvorfor

Å starte et nytt programmeringsprosjekt kan være en spennende og givende opplevelse. Det gir deg muligheten til å utvikle og implementere dine egne ideer, lære nye ferdigheter, og være kreativ. Det er også en flott måte å bygge din portefølje og vise potensielle arbeidsgivere eller klienter hva du er i stand til.

## Slik gjør du det

For å starte et nytt prosjekt i Javascript, må du først velge en kodeeditor som passer for deg. Det finnes mange alternativer der ute, men noen populære valg inkluderer Visual Studio Code, Sublime Text og Atom. Deretter må du installere Node.js og npm, som er nødvendig for å kjøre og administrere dine Javascript-prosjekter.

Når du har satt opp miljøet ditt, kan du begynne å skrive kode. Javascript er et populært programmeringsspråk som brukes til å lage interaktive og dynamiske nettsteder og applikasjoner. For å kunne starte et prosjekt, må du ha grunnleggende kjennskap til Javascript-konsepter som variabler, loops og funksjoner.

La oss for eksempel si at du ønsker å lage en enkel kalkulator. I din kodeeditor, skriv følgende:

```Javascript
// Definerer en funksjon som tar to tall og en operator som parametere
function kalkulator(tall1, tall2, operator) {
  let resultat; 
  // Bruker en switch-case for å utføre riktig matematisk operasjon avhengig av operatøren
  switch (operator) {
    case "+":
      resultat = tall1 + tall2;
      break;
    case "-":
      resultat = tall1 - tall2;
      break;
    case "*":
      resultat = tall1 * tall2;
      break;
    case "/":
      resultat = tall1 / tall2;
      break;
    default:
      console.log("Ugyldig operator");
  }
  // Returnerer resultatet
  return resultat;
}

// Kaller funksjonen og lagrer resultatet i en variabel
let sum = kalkulator(5, 10, "+");

// Skriver ut resultatet i konsollen
console.log(sum); // Forventet utgang: 15
```

Som du kan se, definerer vi her en funksjon som tar inn to tall og en operator som parametere. Deretter bruker vi en switch-case for å utføre riktig matematisk operasjon avhengig av hvilken operator som blir gitt til funksjonen. Til slutt kalles funksjonen og resultatet lagres i en variabel, som deretter blir skrevet ut til konsollen.

## Dypdykk

Når du starter et nytt Javascript-prosjekt, er det viktig å ha en plan og organisere din kode på en god måte. En god praksis er å dele koden din inn i moduler, som er uavhengige deler av koden som utfører en bestemt oppgave. Dette gjør koden enklere å lese, vedlikeholde og feilsøke.

Det er også viktig å dokumentere koden din godt, slik at andre og fremtidige deg selv enkelt kan forstå hva som skjer i koden. Det finnes forskjellige dokumentasjonsverktøy som JSDoc som kan hjelpe deg med dette.

Husk også å inkludere testing i ditt prosjekt. Ved å skrive tester for koden din, kan du sikre at den fungerer som den skal og oppdage eventuelle feil tidlig.

## Se Også

- [Visual Studio Code](https://code.visualstudio.com/)
- [Sublime Text](https://www.sublimetext.com/)
- [Atom](https://atom.io/)
- [Node.js](https://nodejs.org/en/)
- [JSDoc](https://jsdoc.app/)