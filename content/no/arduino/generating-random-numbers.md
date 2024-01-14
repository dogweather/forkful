---
title:                "Arduino: Generering av tilfeldige tall"
programming_language: "Arduino"
category:             "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/arduino/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Hvorfor
Mange prosjekter innen elektronikk og programmering krever bruk av tilfeldige tall. Enten det er for å legge til variasjon i spill, tilfeldig dimming av lys eller å skape unike koder, så kan generering av tilfeldige tall være en viktig del av prosessen. Ved hjelp av Arduino kan du enkelt generere tilfeldige tall for å tilpasse prosjektet ditt og gi det et unikt preg.

## Hvordan
Arduino har en innebygd funksjon for å generere tilfeldige tall, `random()`. Denne funksjonen tar to parametere, hvor den første angir starten på området for de tilfeldige tallene og den andre angir slutten. For å generere et tilfeldig tall mellom 1 og 10, kan du for eksempel bruke følgende kode:

```Arduino 
int tilfeldigTall = random(1, 11); 
```

Dette vil gi deg et tilfeldig heltall mellom 1 og 10. Om du ønsker å generere et tilfeldig desimaltall, kan du bruke funksjonen `random()` sammen med `map()`-funksjonen, som mapper et tall fra en startverdi til en sluttverdi.

```Arduino
float tilfeldigDesimal = map(random(1, 100), 1, 100, 0.0, 1.0);
```

Her vil du få et tilfeldig desimaltall mellom 0.0 og 1.0. Du kan også begrense antall desimaler ved å bruke funksjonen `precision()`. For å generere et tilfeldig tall med to desimaler, kan du bruke følgende kode:

```Arduino
float tilfeldigDesimal = map(random(1, 100), 1, 100, 0.0, 1.0);
tilfeldigDesimal = precision(tilfeldigDesimal, 2);
```

## Dykk dypere
Det finnes også andre måter å generere tilfeldige tall på med Arduino, som for eksempel å bruke spesifikke sensorer eller eksterne enheter. Dette kan være nyttig i prosjekter hvor du ønsker å bruke faktorer som temperatur, lysstyrke eller bevegelse til å generere tilfeldige tall. Du kan også kombinere flere tilfeldige tall for å skape enda mer komplekse og unike tall, for eksempel ved å legge sammen flere randomiseringsfunksjoner.

## Se også
- [Arduino random()](https://www.arduino.cc/reference/en/language/functions/random-numbers/random/)
- [Arduino map()](https://www.arduino.cc/reference/en/language/functions/math/map/)
- [Arduino precision()](https://www.arduino.cc/reference/en/language/variables/data-types/float/precision/)