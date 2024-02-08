---
title:                "Lancement d'un nouveau projet"
aliases:
- fr/arduino/starting-a-new-project.md
date:                  2024-01-20T18:02:51.410489-07:00
model:                 gpt-4-1106-preview
simple_title:         "Lancement d'un nouveau projet"

tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/arduino/starting-a-new-project.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?

Un nouveau projet Arduino, c'est comme un tableau blanc pour un peintre. On démarre des projets pour donner vie à nos idées, résoudre des problèmes ou juste pour s'amuser avec la technologie.

## Comment faire :

Supposons que vous construisez une station météo.

```Arduino
// Incluez les bibliothèques
#include <DHT.h>

// Définissez le type de capteur
#define DHTTYPE DHT22 

// Initialisez le capteur
DHT dht(2, DHTTYPE);

void setup() {
  Serial.begin(9600);
  dht.begin();
}

void loop() {
  // Lisez l'humidité et la température en %
  float humidity = dht.readHumidity();
  float temperature = dht.readTemperature();

  if (isnan(humidity) || isnan(temperature)) {
    Serial.println("Erreur de lecture du DHT");
    return;
  }

  Serial.print("Humidité: ");
  Serial.print(humidity);
  Serial.print("%  Température: ");
  Serial.print(temperature);
  Serial.println("°C ");
  
  delay(2000); // Attendez 2 secondes avant de relire
}
```

Sortie attendue:
```
Humidité: 58.20%  Température: 22.30°C 
```

## Exploration :

L'Arduino est né au début des années 2000 en Italie pour aider les étudiants en design sans expérience en électronique ou en programmation. Alternative à des solutions plus coûteuses comme BASIC Stamp, il est accessible, open source et polyvalent, adapté aussi bien aux débutants qu'aux experts. Pour démarrer un projet, il faut avoir une intention claire, choisir le bon matériel et comprendre les bibliothèques nécessaires à votre projet. Dans notre exemple, la bibliothèque 'DHT.h' est essentielle pour communiquer avec le capteur de température et d'humidité. La structure comprend un `setup()` pour initialiser les paramètres, et un `loop()` qui tourne continuellement.

## Voir également :

- [Documentation Arduino](https://www.arduino.cc/reference/fr/)
- [Projets de démarrage Arduino](https://create.arduino.cc/projecthub)
- [Tutoriels Adafruit pour capteurs DHT](https://learn.adafruit.com/dht)
