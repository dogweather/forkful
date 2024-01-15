---
title:                "Å starte et nytt prosjekt"
html_title:           "Gleam: Å starte et nytt prosjekt"
simple_title:         "Å starte et nytt prosjekt"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/gleam/starting-a-new-project.md"
---

{{< edit_this_page >}}

# Hvorfor

Det å starte et nytt prosjekt kan være spennende og utfordrende. Det gir deg muligheten til å skape noe nytt, lære nye teknologier og utvide dine ferdigheter som programmerer. Med Gleam som ditt valgte programmeringsspråk, vil du oppleve en elegant og driftssikker måte å utvikle programvare på.

# Hvordan

La oss ta en titt på hvordan du kan komme i gang med et nytt prosjekt i Gleam. Først må du installere Gleam Compiler ved å følge instruksjonene på deres offisielle nettside. Deretter kan du følge disse trinnene for å opprette ditt første prosjekt:

1. Åpne terminalen din og naviger til mappen der du ønsker å opprette prosjektet ditt.
2. Skriv inn følgende kommando for å initialisere et nytt Gleam-prosjekt:
```Gleam new project_name```
3. Dette vil opprette en ny mappe kalt "project_name" med noen filer og mapper inne i den.
4. Åpne mappen i din favoritt teksteditor og du er klar til å begynne å skrive din første Gleam-kode!

Her er et eksempel på en enkel "Hello World!" -applikasjon skrevet i Gleam:

```Gleam
pub struct Person(name)
fn greeting(person: Person) {
  let message = "Hello, " ++ person.name ++ "!"
  IO.print(message)
}
pub fn main() {
  let person = Person("John")
  greeting(person)
}
```

Dette vil gi følgende output når det kjøres:

```
Hello, John!
```

# Dypdykk

Når du har opprettet et nytt prosjekt i Gleam, vil du bli presentert med en "TODO" -liste med punkter som må fullføres for å få prosjektet ditt til å kjøre. Dette kan inkludere å skrive moduler, funksjoner og testing av koden din.

En av de viktigste aspektene ved å utvikle i Gleam er dets sterke type-system. Dette betyr at kodefeil kan oppdages tidlig og koden din vil være mer robust og sikrere.

Det er også verdt å nevne at Gleam har et vennlig og dedikert samfunn som er mer enn villig til å hjelpe deg med eventuelle spørsmål du måtte ha.

# Se også

- Offisiell Gleam-nettside: https://gleam.run/
- Gleam Compiler installasjonsinstruksjoner: https://gleam.run/getting-started/
- Gleams offisielle dokumentsjon: https://gleam.run/documentation/