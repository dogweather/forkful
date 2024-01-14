---
title:    "Arduino: Skriving til standardfeil"
keywords: ["Arduino"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/arduino/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Hvorfor

Et av de mest vanlige spørsmålene som nybegynnere har når de begynner å lære å programmere Arduino er - hvorfor skal jeg skrive til standard error? Det er en god praksis å følge når du skriver kode, og i denne artikkelen vil vi forklare hvorfor det er viktig og hvordan du kan gjøre det.

## Hvordan man gjør det

For å skrive til standard error i Arduino, kan du bruke funksjonen ```Serial.print()```. Denne funksjonen tar en tekststreng eller tall som parameter og sender det til seriellporten. For eksempel, hvis du vil skrive ut teksten "Hei, verden!" til standard error, kan du bruke følgende kode:

```Arduino
Serial.print("Hei, verden!");
```

Når du kjører denne koden, vil teksten "Hei, verden!" bli sendt til seriellporten og vises i seriellmonitorvinduet. 

## Dykk dypere

Så hvorfor er det viktig å skrive til standard error? Det er fordi når du skriver til standard error, skiller du ut feilmeldinger fra standard utskrift. Dette gjør det lettere å finne og feilsøke problemer i koden din. Ved å få feilmeldingene separert, kan du fokusere mer på å forbedre koden din enn å lete etter feil.

En annen fordel med å skrive til standard error er at du kan sende variabler og annen informasjon som er nyttig for feilsøking til seriellmonitorvinduet. Dette kan hjelpe deg med å forstå hva som skjer med koden din og hvor problemet ligger.

## Se Også

Du kan lese mer om Serial.print() funksjonen og hvordan du kan bruke den på Arduino sin offisielle nettside: https://www.arduino.cc/reference/en/language/functions/communication/serial/print/ 

For mer informasjon om feilsøking i Arduino, kan du sjekke ut denne guiden fra Arduino-prosjektet: https://www.arduino.cc/en/Guide/Troubleshooting 

Og hvis du vil lære mer om Arduino-programmering generelt, kan du ta en titt på våre andre artikler på [Nettsiden] (https://www.nettsted.no/arduino-programmering). Lykke til med å skrive til standard error i dine fremtidige prosjekter!