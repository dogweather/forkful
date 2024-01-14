---
title:                "Arduino: Utskrift av feilsøkingsresultater"
simple_title:         "Utskrift av feilsøkingsresultater"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/arduino/printing-debug-output.md"
---

{{< edit_this_page >}}

##Hvorfor

Når du arbeider med Arduino-programmering kan det noen ganger være vanskelig å forstå hvorfor noe ikke fungerer som det skal. Det er her utskrift av feilmeldinger og debug-informasjon kommer til nytte. Ved å skrive ut denne informasjonen kan du få en bedre forståelse av hva som skjer i koden din og dermed feilrette problemet mer effektivt.

##Slik gjør du det

For å skrive ut debug-informasjon i Arduino-kode, kan du bruke funksjonen "Serial.print ()". Dette vil skrive ut informasjonen du ønsker i seriell overvåkingsvindu i Arduino IDE.

```Arduino Serial.print ("Hei, dette er en debug-melding!");```

Dette vil skrive ut teksten "Hei, dette er en debug-melding!" i overvåkningsvinduet hver gang koden din når denne linjen.

##Dypere dykk

For å få mer detaljert informasjon i utskriftene dine kan du bruke funksjonen "Serial.println ()". Dette vil skrive ut meldingen din i et eget linjeskift i overvåkningsvinduet.

```Arduino Serial.println ("Verdi av variabel: " + variabel);```

Dette vil skrive ut verdien av variabelen din sammen med teksten "Verdi av variabel: " i et eget linjeskift i overvåkningsvinduet.

Du kan også bruke bindestrek for å kombinere tekst og variabler i utskriftene dine.

```Arduino Serial.println ("Summen av tallene er: " + tall1 + tall2);```

Dette vil skrive ut summen av verdien av tall1 og tall2 sammen med teksten "Summen av tallene er: " i et eget linjeskift i overvåkningsvinduet.

##Se også

- Mer informasjon om Serial-funksjonene: [https://www.arduino.cc/reference/en/language/functions/communication/serial/](https://www.arduino.cc/reference/en/language/functions/communication/serial/)
- Tutorial om debugging i Arduino: [https://www.arduino.cc/en/Tutorial/Debugging](https://www.arduino.cc/en/Tutorial/Debugging)