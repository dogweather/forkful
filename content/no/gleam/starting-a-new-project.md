---
title:                "Gleam: Å starte et nytt prosjekt."
programming_language: "Gleam"
category:             "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/gleam/starting-a-new-project.md"
---

{{< edit_this_page >}}

## Hvorfor

Å starte et nytt prosjekt kan være både spennende og utfordrende. Det gir deg muligheten til å utforske nye ideer og lære nye ferdigheter. Gleam er et programmeringsspråk som kan hjelpe deg med å realisere dine kreative visjoner og bygge utrolige applikasjoner. Hvis du er klar for å ta spranget og starte et nytt Gleam-prosjekt, så er denne artikkelen for deg.

## Hvordan

For å starte et nytt prosjekt i Gleam, må du først installere Gleam compiler og byggeverktøyet. Når det er gjort, kan du følge disse trinnene:

1. Åpne en terminal og gå til mappen der du vil lagre prosjektet.
2. Skriv inn kommandoen `gleam new <prosjektnavn>` for å opprette en ny Gleam-applikasjon.
3. Naviger inn i den nye mappen ved å skrive `cd <prosjektnavn>`.
4. Du er nå klar til å starte å kode!

Her er et eksempel på en enkel "Hello World" -applikasjon i Gleam:

```Gleam
pub fn main() {
  let beskjed = "Hei verden!"
  io.println(beskjed)
}
```

Kjør koden ved å skrive `gleam run` i terminalen. Du bør se utskriften `Hei verden!`.

Du kan også følge med i offisiell Gleam dokumentasjon for flere eksempler og detaljert informasjon om hvordan du starter et nytt prosjekt.

## Dypdykk

Når du har forstått det grunnleggende i å starte et nytt Gleam-prosjekt, kan du begynne å utforske mer komplekse funksjoner og konsepter. Gleam har støtte for funksjonell programmering, mønstergjenkjenning og typet sikkerhet, noe som betyr at det gir en robust og pålitelig måte å utvikle applikasjoner på.

Når du utvikler i Gleam, bør du også vurdere å bruke biblioteker og rammeverk som kan hjelpe deg med å akselerere utviklingsprosessen. For eksempel kan du bruke Phoenix biblioteket for å bygge webapplikasjoner i Gleam. Ved å eksperimentere og leke med ulike verktøy og muligheter, kan du oppdage nye måter å bygge spennende og innovative applikasjoner på.

## Se også

- Offisiell Gleam dokumentasjon: https://gleam.run/
- Gleam på GitHub: https://github.com/gleam-lang/gleam