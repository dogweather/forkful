---
title:                "Calculer une date dans le futur ou le passé"
html_title:           "Arduino: Calculer une date dans le futur ou le passé"
simple_title:         "Calculer une date dans le futur ou le passé"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/arduino/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Qu'est-ce que c'est et pourquoi faire?

Calculer une date dans le futur ou dans le passé est l'action de prédire une date future ou passée à partir d'une date de départ et d'une durée de temps spécifique. Les programmeurs le font principalement pour planifier des événements ou pour créer des applications qui gèrent des dates et des délais.

## Comment faire:

Voici un exemple simple de code Arduino pour calculer une date dans le futur:

```arduino

unsigned long now = millis(); //récupère l'heure actuelle en millisecondes
unsigned long futureDate = now + 1000000; //ajoute 1000000 millisecondes pour calculer la date future
Serial.print(F("La date future est: "));
Serial.println(futureDate); //affiche la date future en millisecondes
```

La sortie de ce code serait: "La date future est: 1000000". Vous pouvez également convertir les millisecondes en une date lisible en utilisant la fonction ```millisToDays```.

## Plongée en profondeur:

Historiquement, la réalisation de calculs de dates dans les programmes était difficile et sujette à des erreurs. Heureusement, il existe aujourd'hui de nombreuses bibliothèques et fonctions mathématiques pour faciliter ce processus. Les programmeurs peuvent également utiliser des modules d'horloge en temps réel pour obtenir une heure précise et ainsi mieux gérer les conversions de dates.

## Voir également:

- [Tutoriel pour les bibliothèques de temps et de date Arduino](https://www.arduino.cc/en/Tutorial/BuiltinExamples/Libraries/DateTime)
- [Bibliothèque Time pour Arduino](https://github.com/PaulStoffregen/Time)
- [Explications sur la conversion de millisecondes en date](https://www.instructables.com/id/Converting-milliseconds-to-date-in-arduino/)