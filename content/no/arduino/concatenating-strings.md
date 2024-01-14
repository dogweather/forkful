---
title:    "Arduino: Sammenstilling av strenger"
keywords: ["Arduino"]
---

{{< edit_this_page >}}

# Hvorfor

Å slå sammen strenger (informatikk begrep for å kombinere to eller flere tekststrenger) i Arduino programmering kan være nyttig for å opprette mer kompleks utskrift og meldinger. Ved å bruke denne funksjonen i koden din, kan du også gi dynamiske og personlige meldinger fra enheten din.

# Hvordan

Kombinering av strenger i Arduino er enkelt å gjøre ved å bruke funksjonen `String.concat()`. Denne funksjonen lar deg kombinere to eller flere strenger sammen, og skrive ut dem som én komplett streng. Her er et eksempel:

```arduino
void setup() {
  Serial.begin(9600);
}

void loop() {
  String navn = "John";
  String beskjed = "Hei " + navn + ", velkommen til Arduino-verdenen!";
  Serial.println(beskjed);
}
```

I dette eksempelet blir strengen `navn` kombinert med teksten "Hei" og teksten ", velkommen til Arduino-verdenen!". Dette vil resultere i en utskrift som sier "Hei John, velkommen til Arduino-verdenen!".

Du kan også legge til flere variabler og tekst sammen. For eksempel, hvis du vil inkludere et tall i meldingen din, kan du gjøre det på denne måten:

```arduino
void setup() {
  Serial.begin(9600);
}

void loop() {
  int alder = 25;
  String beskjed = "Gratulerer med " + String(alder) + "-årsdagen!";
  Serial.println(beskjed);
}
```

I dette eksempelet blir tallet `alder` først konvertert til en streng ved å bruke `String()` funksjonen, og deretter kombinert med teksten "Gratulerer med" og teksten "-årsdagen!". Dette vil resultere i en utskrift som sier "Gratulerer med 25-årsdagen!".

# Dykk dypere

I tillegg til å bruke `String.concat()` funksjonen, kan du også bruke operatøren `+` for å kombinere strenger. Dette er det samme som å bruke `String.concat()` funksjonen, men i en kortere form. Her er et eksempel:

```arduino
void setup() {
  Serial.begin(9600);
}

void loop() {
  int antall = 15;
  String beskjed = "Du har " + String(antall) + " nye meldinger.";
  Serial.println(beskjed);
}
```

I dette eksempelet blir tallet `antall` først konvertert til en streng, og deretter kombinert med teksten "Du har" og teksten "nye meldinger.". Dette vil resultere i en utskrift som sier "Du har 15 nye meldinger.".

# Se også

* [String.concat() referanse](https://www.arduino.cc/reference/en/language/functions/string/functions/concat/)
* [Arduino opplæring om strenger](https://www.arduino.cc/en/Tutorial/StringConstructors)
* [Arduino Community Forum](https://forum.arduino.cc/) for å lære mer og få hjelp fra andre Arduino-entusiaster.