---
title:                "Willekeurige getallen genereren"
date:                  2024-01-28T22:00:30.332553-07:00
model:                 gpt-4-0125-preview
simple_title:         "Willekeurige getallen genereren"

category:             "Arduino"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/arduino/generating-random-numbers.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?
Het genereren van willekeurige getallen in Arduino-projecten houdt in dat waarden worden geproduceerd die ontworpen zijn om onvoorspelbaar te zijn, cruciaal voor toepassingen zoals games, simulaties en beveiligingssystemen. Programmeurs gebruiken deze techniek om variabiliteit te introduceren of beslissingen te nemen die niet deterministisch mogen zijn.

## Hoe te:
Arduino biedt eenvoudige functies voor het genereren van willekeurige getallen: `randomSeed()` en `random()`. Om te beginnen, initialiseer je de generator van willekeurige getallen om ervoor te zorgen dat je elke keer dat je programma draait verschillende getallenreeksen krijgt. Een vaak gebruikte benadering is om te zaaien met een analoge lezing van een niet-aangesloten pin.

```Arduino
void setup() {
  Serial.begin(9600);
  // Initialiseer willekeurige zaad
  randomSeed(analogRead(0));
}

void loop() {
  // Genereer een willekeurig getal tussen 0 en 99
  int randomNumber = random(100);
  Serial.println(randomNumber);
  delay(1000); // Vertraag een seconde voor de leesbaarheid van de output
}
```

Het bovenstaande programma initialiseert de generator van willekeurige getallen in de `setup()` functie en genereert bij elke lusiteratie een nieuw getal tussen 0 en 99, waarbij het getal naar de Seriële Monitor wordt gestuurd.

Voorbeelduitvoer:
```
42
17
93
...
```

## Diepgaande duik
De `random()` functie van Arduino maakt onder de motorkap gebruik van een pseudo-willekeurige getallengenerator (PRNG), die een deterministische reeks volgt maar statistisch willekeurig lijkt. De beginwaarde, of zaad, van de reeks beïnvloedt sterk de onvoorspelbaarheid ervan, vandaar het veelvuldig gebruik van `randomSeed()` met een enigszins willekeurige invoer als startpunt. Het is belangrijk om op te merken dat de door Arduino gegenereerde willekeurigheid voldoende is voor de meeste hobbyprojecten, maar mogelijk niet voldoet aan de criteria voor toepassingen met hoge beveiliging vanwege de voorspelbaarheid op lange termijn. Voor cryptografische doeleinden is het raadzaam om te kijken naar meer gesofisticeerde algoritmen en hardware willekeurige getallengeneratoren (HRNG's), die echte willekeurigheid kunnen bieden door gebruik te maken van fysieke processen.
