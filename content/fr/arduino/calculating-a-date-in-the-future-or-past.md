---
title:                "Calcul d'une date dans le futur ou le passé"
html_title:           "Arduino: Calcul d'une date dans le futur ou le passé"
simple_title:         "Calcul d'une date dans le futur ou le passé"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/arduino/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

# Pourquoi

Saviez-vous qu'il est possible de calculer une date dans le futur ou dans le passé en utilisant votre Arduino ? Cela peut être très utile pour les projets impliquant des horaires spécifiques ou des délais précis. Dans cet article, nous allons vous montrer comment réaliser ce calcul en toute simplicité.

## Comment faire

Pour calculer une date dans le futur ou dans le passé, nous devons d'abord déterminer la date de départ, puis spécifier le nombre de jours à ajouter ou soustraire. Voici un exemple de code pour calculer une date dans le futur en ajoutant 30 jours à la date actuelle :

```
Arduino
#include <Time.h> // inclure la bibliothèque Time

void setup() {
  // initialiser la connexion série
  Serial.begin(9600);

  // définir la date de départ au format YYYY,MM,DD
  tmElements_t t;
  t.Year = 2021;
  t.Month = 11;
  t.Day = 30;

  // ajouter 30 jours à la date de départ
  time_t futur = makeTime(t) + (30 * 24 * 60 * 60);

  // convertir le temps en date
  tmElements_t date = breakTime(futur);

  // afficher la date calculée
  Serial.print("La date dans 30 jours sera : ");
  Serial.print(date.Year);
  Serial.print("/");
  Serial.print(date.Month);
  Serial.print("/");
  Serial.println(date.Day);
}

void loop() {
  // ne rien faire ici
}
```

Lorsque vous téléversez ce code sur votre Arduino et ouvrez le moniteur série, vous devriez voir la date calculée s'afficher : 2021/12/30.

Vous pouvez également modifier le code pour calculer une date dans le passé en soustrayant un certain nombre de jours à la date de départ. Il suffit de changer la ligne pour ajouter le nombre de jours souhaité :

```
// soustraire 30 jours à la date de départ
time_t passe = makeTime(t) - (30 * 24 * 60 * 60);
```

## Plongée en profondeur

Le code présenté ici utilise la bibliothèque Time pour gérer les calculs de date et de temps sur votre Arduino. Cette bibliothèque est très utile car elle permet de convertir facilement des informations de date et de temps en valeurs numériques.

Pour ajouter ou soustraire des jours, nous utilisons l'opérateur mathématique de multiplication (*) pour convertir le nombre de jours en secondes (1 jour = 24 heures = 24 * 60 minutes = 24 * 60 * 60 secondes). Nous pouvons ensuite utiliser la fonction makeTime() pour créer une valeur de temps à partir de notre structure de date créée avec les informations de la date de départ.

## Voir aussi

Si vous souhaitez en savoir plus sur la manipulation des dates et du temps avec votre Arduino, voici quelques liens utiles :

- [Documentation officielle de la bibliothèque Time](https://playground.arduino.cc/Code/Time/)
- [Tutoriel sur la gestion du temps avec Arduino](https://randomnerdtutorials.com/guide-for-real-time-clock-rtc/)
- [Utiliser une horloge temps réel (RTC) avec votre Arduino](https://learn.adafruit.com/adafruit-ds1307-real-time-clock-breakout-board-kit/overview)

Maintenant que vous savez comment calculer une date dans le futur ou dans le passé avec votre Arduino, vous pouvez l'utiliser dans vos projets pour ajouter plus de précision et de flexibilité à vos horaires. À vous de jouer !