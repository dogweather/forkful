---
title:                "Feilhåndtering"
date:                  2024-01-26T00:37:14.975259-07:00
model:                 gpt-4-1106-preview
simple_title:         "Feilhåndtering"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/arduino/handling-errors.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Feilhåndtering i programmene dine fanger opp de uforutsette tingene som vil prøve å snuble deg opp. Du gjør det for å hindre at Arduinoen din får et sammenbrudd når det uventede skjer.

## Hvordan gjøre det:

La oss si at Arduinoen din leser en sensor som kan produsere verdier utenfor rekkevidden av og til. Slik kan du håndtere det:

```Arduino
int sensorVerdi = analogRead(A0);

if (sensorVerdi >= 0 && sensorVerdi <= 1023) {
  // Verdien er innenfor rekkevidde, fortsett med behandlingen
  Serial.println(sensorVerdi);
} else {
  // Verdien er utenfor rekkevidde, håndter feilen
  Serial.println("Feil: Sensorverdi utenfor rekkevidde.");
}
```
Eksempel på utskrift:
```
523
Feil: Sensorverdi utenfor rekkevidde.
761
```

## Dypdykk

Feilhåndtering har ikke alltid vært så greit. I de tidlige dagene ignorerte ofte utviklere feil, noe som førte til den fryktede "udefinerte oppførselen". Etter hvert som programmering utviklet seg, ble verktøyene forbedret — du har nå unntak i mange språk, men i Arduino-verdenen er det fremdeles en gammeldags 'sjekk-først' på grunn av maskinvarebegrensninger og C++ røtter.

I Arduino-programmering ser man ofte `if-else`-setninger for feilhåndtering. Men det finnes alternativer: å bruke `assert`-funksjonen for å stoppe utførelsen hvis en betingelse mislykkes, eller å designe feilsikre mekanismer inne i selve maskinvareoppsettet.

Når du implementerer feilhåndtering, bør du vurdere effekten av å stoppe programmet kontra å tillate det å fortsette med en standard eller sikker tilstand. Det er en avveiing, og det riktige valget avhenger av potensiell skade ved avbrudd kontra feilbetjening.

## Se også

Friske opp feildeteksjon og håndtering med disse:

- Arduino Språkreferanse: https://www.arduino.cc/reference/en/
- Embedded Artistrys dypere titt inn i feilhåndtering: https://embeddedartistry.com/blog/2017/05/17/creating-a-circular-buffer-in-c-and-c/
- C++ Feilhåndtering: https://en.cppreference.com/w/cpp/error/exception

Dette bør gi deg kunnskapen og selvtilliten til å unngå fallgrubene av feil i dine Arduino-eventyr.
