---
title:    "Arduino: Å starte et nytt prosjekt"
keywords: ["Arduino"]
---

{{< edit_this_page >}}

##Hvorfor

Å starte et nytt prosjekt med Arduino er en spennende og givende opplevelse. Du vil kunne lære om programmering, elektronikk og kreativ problemløsning på en praktisk måte. Det er også en flott måte å bygge noe unikt og personlig som du kan dele med verden.

##Slik gjør du det

Først må du sørge for at du har en Arduino mikrokontroller og tilhørende programvare. Du kan kjøpe et startsett som inkluderer alt du trenger, eller du kan kjøpe de ulike komponentene separat.

For å komme i gang, må du lage en enkel krets med LED-lys og en motstand. Koblingsskjema og kodeeksempel for dette kan du finne på internett. Når du har forstått grunnleggende koding med Arduino, kan du begynne å eksperimentere med ulike sensorer og moduler for å lage mer avanserte prosjekter.

```Arduino
int ledPin = 13; // setter LED til pin 13
void setup() {
  pinMode(ledPin, OUTPUT); // setter LED-pin til OUTPUT mode
}
void loop() {
  digitalWrite(ledPin, HIGH); // slår på LED-lyset
  delay(1000); // venter i ett sekund
  digitalWrite(ledPin, LOW); // slår av LED-lyset
  delay(1000); // venter i ett sekund
}
```

Koden over vil få LED-lyset til å blinke av og på hvert sekund. Du kan eksperimentere med å endre tiden for lys på og av, og også legge til flere LED-lys i kretsen.

##Dypdykk

Når du føler deg komfortabel med å lage enkle kretser og kode i Arduino, kan du begynne å tenke på større og mer komplekse prosjekter. Dette kan være alt fra å bygge en robot eller en egen hjemmeautomatiseringssystem. Det finnes mange ressurser på nettet som kan hjelpe deg med å utvikle dine ferdigheter og inspirere deg til å bygge noe unikt.

Husk å alltid være tålmodig og ta deg tid til å forstå hvordan komponentene fungerer sammen. Det er også lurt å ta små steg og teste koden underveis, slik at du kan feilsøke og forbedre prosjektet ditt underveis.

##Se også

- [Arduino sin offisielle nettside](https://www.arduino.cc/)
- [Hvordan bygge en enkel krets med Arduino](https://www.instructables.com/id/Arduino-Projects/)
- [Mer avanserte kretser og prosjekter med Arduino](https://www.arduino.cc/reference/en/)
- [Arduino fellesskapets forum hvor du kan få hjelp og inspirasjon](https://forum.arduino.cc/)