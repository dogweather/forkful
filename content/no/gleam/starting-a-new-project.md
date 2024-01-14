---
title:    "Gleam: Å starte et nytt prosjekt"
keywords: ["Gleam"]
---

{{< edit_this_page >}}

## Hvorfor

Hvorfor skulle noen ønske å begynne på et nytt prosjekt? Det kan være mange grunner, men noen vanlige er å forbedre eksisterende funksjonalitet, lære nye programmeringsspråk og teknikker, eller å skape noe som kan hjelpe andre eller gi en personlig utfordring. Uansett grunn, er det alltid spennende å starte på et nytt prosjekt og se hvor det fører deg.

## Slik gjør du det

Nå som du har bestemt deg for å starte et nytt prosjekt, la oss se på noen enkle kodestumper i Gleam for å komme i gang.

```Gleam
pub struct Person(age, name) {
  fn new(age, name) {
    Person(age, name)
  }
}

fn greet(person) {
  "Hei " + person.name + " er " + person.age + " år gammel."
}

let person = Person.new(25, "Ole");
greet(person)
```

Her har vi definert en enkel Person-struktur med en alder og navn. Deretter har vi en funksjon som tar inn en Person-struktur og bruker informasjonen til å lage en enkel tekststreng. Til slutt har vi opprettet en ny person og brukt greet-funksjonen til å skrive ut en hilsen.

Dette er bare et eksempel på hvordan du kan starte å jobbe med Gleam. Det er mange flere muligheter og funksjoner du kan bruke i prosjektet ditt, så ikke nøl med å utforske!

## Dypdykk

Nå som du har lært det grunnleggende for å starte et nytt Gleam-prosjekt, la oss se på noen dypere detaljer. For å starte et prosjekt må du ha en grundig forståelse av Gleam-syntaxen og hvordan du kan bruke funksjonaliteten til å bygge noe unikt.

I tillegg er det viktig å forstå hvordan du kan organisere prosjektet ditt med riktige moduler og filstrukturer, og hvordan du kan integrere eventuelle avhengigheter du kanskje trenger.

Det er også viktig å lese dokumentasjonen nøye for å forstå alle funksjoner og aspekter ved Gleam-programmeringsspråket.

## Se også

- Offisiell Gleam-dokumentasjon: https://gleam.run/
- Gleam GitHub Repository: https://github.com/gleam-lang/gleam
- Eksempler på Gleam-prosjekter: https://gleam.run/examples/
- Gleam-samfunnets Slack-kanal: https://gleam-run.slack.com/