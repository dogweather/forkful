---
changelog:
- 2024-01-30, gpt-4-0125-preview, translated from English
date: 2024-01-30 19:09:55.187301-07:00
description: "I Arduino-v\xE4rlden l\xE5ter associativa arrayer dig koppla nycklar\
  \ till v\xE4rden, lite som att du skulle matcha sockor med deras par. De \xE4r ett\
  \ givet val n\xE4r du\u2026"
lastmod: '2024-03-13T22:44:38.161192-06:00'
model: gpt-4-0125-preview
summary: "I Arduino-v\xE4rlden l\xE5ter associativa arrayer dig koppla nycklar till\
  \ v\xE4rden, lite som att du skulle matcha sockor med deras par."
title: "Att anv\xE4nda associativa arrayer"
weight: 15
---

## Vad & Varför?
I Arduino-världen låter associativa arrayer dig koppla nycklar till värden, lite som att du skulle matcha sockor med deras par. De är ett givet val när du behöver lagra och hämta data med beskrivande namn, vilket gör din kod renare och mycket mer förståelig.

## Hur gör man:
Arduino har, strikt talat, inte inbyggt stöd för associativa arrayer som du skulle hitta i högre programmeringsspråk. Men, frukta inte. Vi kan bli uppfinningsrika genom att använda strukturer och arrayer för att efterlikna denna funktionalitet. Här är ett enkelt exempel på att skapa en grundläggande "associativ array" för att lagra och komma åt temperaturer för olika städer.

Först, definiera en struktur för att hålla staden (nyckeln) och dess temperatur (värdet):

```cpp
struct CityTemperature {
  String city;
  float temperature;
};
```

Nästa steg, initialisera en array av `CityTemperature`-objekt:

```cpp
CityTemperature temperatures[] = {
  {"New York", 19.5},
  {"Los Angeles", 22.0},
  {"Chicago", 17.0}
};
```

Så här kan du komma åt och visa temperaturen för en specifik stad:

```cpp
void setup() {
  Serial.begin(9600);
  for(int i = 0; i < 3; i++) {
    if(temperatures[i].city == "Los Angeles") {
      Serial.print("Temperaturen i Los Angeles är: ");
      Serial.println(temperatures[i].temperature);
    }
  }
}

void loop() {
  // Inget här just nu.
}
```

Att köra denna kod skulle ge dig utdatan:

```
Temperaturen i Los Angeles är: 22.0
```

## Fördjupning
Historiskt sett kom programmeringsspråk som C och C++ (från vilka Arduino-syntax är härledd) inte med inbyggda associativa arrayer, vilket ledde till arbetsrundor som den som visas ovan. Denna ansats är relativt enkel men skalar dåligt när datamängden ökar på grund av dess O(n) söktid.

Språk som Python erbjuder ordböcker, och JavaScript har objekt för detta syfte, båda är mycket mer effektiva för att hantera nyckel-värdepar. I Arduino, när prestanda och effektivitet blir kritiska, kan utvecklare välja att använda mer specialiserade datastrukturer, som hashtabeller, implementerade via bibliotek.

Även om Arduino inte inbyggt stöder associativa arrayer, har gemenskapen utvecklat bibliotek som `HashMap` som kan läggas till i ditt projekt för att erbjuda liknande funktionalitet med bättre prestanda än ett DIY-ansats. Dessa bibliotek erbjuder vanligtvis ett mer elegant och effektivt sätt att hantera associativa arrayer, speciellt för mer komplexa projekt.
