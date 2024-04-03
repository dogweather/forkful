---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:06:40.396795-07:00
description: 'Hoe: In Arduino kunt u getallen afronden met ingebouwde functies. Belangrijke
  spelers zijn `round`, `ceil`, en `floor`. Hier is een snelle demo.'
lastmod: '2024-03-13T22:44:51.067585-06:00'
model: gpt-4-0125-preview
summary: In Arduino kunt u getallen afronden met ingebouwde functies.
title: Afronden van getallen
weight: 13
---

## Hoe:
In Arduino kunt u getallen afronden met ingebouwde functies. Belangrijke spelers zijn `round`, `ceil`, en `floor`. Hier is een snelle demo:

```arduino
void setup() {
  Serial.begin(9600);
  
  float myNumber = 123.4567;

  // Afronden naar het dichtstbijzijnde gehele getal
  Serial.println(round(myNumber)); // Resultaat: 123

  // Altijd afronden naar boven
  Serial.println(ceil(myNumber));  // Resultaat: 124

  // Altijd afronden naar beneden
  Serial.println(floor(myNumber)); // Resultaat: 123
}

void loop() {
  // Niets om doorheen te lopen.
}
```

## Diepere Duik:
Afrondalgoritmen hebben een lange geschiedenis; ze bestonden al lang voordat digitale computers er waren. In analoge computing was afronden een fysiek proces. In digitale computing is het een wiskundig proces.

Afronden is nodig wanneer we converteren van een type met meer precisie (zoals `float` of `double`) naar een type met minder precisie (zoals `int`). Maar hoe we afronden kan variëren:

1. `round()`: Standaard afronding. Als de fractie 0,5 of hoger is, gaat het omhoog; anders gaat het naar beneden.
2. `ceil()`: Afkorting voor "plafond", rondt altijd af naar boven naar het dichtstbijzijnde hele getal, zelfs als het dichter bij het lagere getal is.
3. `floor()`: Tegenovergestelde van plafond; rondt altijd af naar beneden.

Het kiezen tussen deze functies hangt af van waarvoor de afgeronde waarde is bedoeld. Metingen kunnen standaard afronding nodig hebben, geld gebruikt vaak `floor`, terwijl inventarissystemen `ceil` kunnen gebruiken om ervoor te zorgen dat alles is meegerekend.

De implementatie van deze functies in Arduino is rechttoe rechtaan; ze behandelen geen extra gevallen zoals afronden naar specifieke decimalen. Daarvoor komt een aangepaste functie of diepere wiskunde kijken—denk aan vermenigvuldigen om de decimaal te verschuiven, afronden, en dan terug delen.

Afrondingsfouten kunnen zich opstapelen, wat een aanzienlijke impact kan hebben op lange berekeningen of iteratieve processen. Programmeurs moeten voorzichtig zijn bij het uitvoeren van talrijke operaties op afgeronde waarden.

## Zie Ook:
2. Diepgaande blik op de valkuilen en strategieën voor afronden: [Gids voor Drijvende Komma](https://floating-point-gui.de/)
3. Voor geavanceerde technieken, inclusief aangepaste afrondingsfuncties en het omgaan met afrondingsfouten, kunt u academische bronnen of gedetailleerde programmeringsgidsen raadplegen.
