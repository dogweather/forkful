---
title:                "Skriver til standard feil"
html_title:           "Javascript: Skriver til standard feil"
simple_title:         "Skriver til standard feil"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/javascript/writing-to-standard-error.md"
---

{{< edit_this_page >}}

# Hva og hvorfor?

Skriver du noen ganger til standardfeil når du koder? Det betyr rett og slett at man sender feilmeldinger eller annen informasjon til strømmen for standardfeil i stedet for standardutgang. Programmerere gjør dette fordi det er et viktig verktøy for å feilsøke og forbedre koden sin.

# Hvordan:

```Javascript
console.error("Dette er en feilmelding til standardfeil!");
```

Resultat:
Dette er en feilmelding til standardfeil!

# Dypdykk:

Det å skrive til standardfeil er en praksis som går helt tilbake til Unix-operativsystemet på 1970-tallet. Det ble utviklet som en måte å håndtere feil og uventede situasjoner på. Det finnes også alternative måter å håndtere feilmeldinger på, som for eksempel å kaste unntak eller bruke loggfiler. Når man skriver til standardfeil, kan man også inkludere informasjon som hjelper til med feilsøking, som for eksempel variabler og stakkspor.

# Se også:

For å lære mer om å skrive til standardfeil i Javascript, kan du sjekke ut disse kildene:

- [MDN Web Docs](https://developer.mozilla.org/en-US/docs/Web/API/Console/error)