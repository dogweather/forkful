---
title:    "Arduino: Lesing av kommandolinje-argumenter"
keywords: ["Arduino"]
---

{{< edit_this_page >}}

## Hvorfor

Mange starter sin reise inn i programmering ved å lære et språk som Python eller Java. Men med sin enkle og brukervennlige plattform, er Arduino en flott måte å utforske verden av elektronikk og mikrokontrollere. For å utnytte hele potensialet til Arduino, er det viktig å forstå hvordan man leser kommandolinje-argumenter.

## Hvordan

Arduino bruker en kommandolinje-looper for å kjøre programvaren, og dette kan bli brukt til å lese kommandolinje-argumenter. La oss ta en titt på et enkelt eksempel:

```Arduino
void setup() {
  Serial.begin(9600);                         // Starter seriell kommunikasjon
  while (!Serial) {
    ;                                         // Venter på seriell tilkobling
  }

  Serial.print("Kommandolinje-argumenter: "); // Skriver ut tekst før argumentene
  Serial.println(Serial.readString());        // Leser og skriver ut kommandolinje-argumentene
}

void loop() {
  // Ingenting her, da vi bare vil lese kommandolinje-argumenter én gang
}
```

Når du laster opp dette til Arduino-enheten din og åpner Serial Monitor, vil du se en linje som sier "Kommandolinje-argumenter:", fulgt av argumentene du skrev inn etter at du startet programmet. For eksempel, hvis du skrev "test" som argument, vil du se "Kommandolinje-argumenter: test" i Serial Monitor.

## Dypdykk

Så hvordan fungerer dette egentlig? Når du starter et program på Arduino, sender kommandolinjen alle argumentene til datamaskinen, som deretter overfører dem til Arduino via den serielle kommunikasjonsporten. I programmet vårt, bruker vi ``Serial.readString()`` for å lese argumentene og skrive dem ut i Serial Monitor.

Ettersom vi bruker ``Serial.print`` og ``Serial.println``, vil argumentene bli lagt til i slutten av linjen i stedet for å overskrive den. Dette er nyttig hvis du for eksempel vil skrive ut en "hei <navn>" melding basert på argumentet du gir.

En annen viktig ting å merke seg er at kommandolinje-argumentene må være adskilt med mellomrom. Hvis du for eksempel skriver "test1,test2,test3" som argumenter, vil alle tre argumentene bli slått sammen til en streng og skrives ut som en.

## Se også

* [Kommandolinje-argumenter i Arduino Offisiell Dokumentasjon](https://www.arduino.cc/reference/en/language/functions/communication/serial/readstring/)
* [Serial Monitor tutorial på Arduino Project Hub (på norsk)](https://create.arduino.cc/projecthub/Arduino_Genuino/seriell-monitor-466d9b)
* [Hvorfor begynne å lære Arduino? (på norsk)](https://engineering.thrysoee.net/2013/01/31/tutorial-why-start-learning-arduino/)