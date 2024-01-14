---
title:                "Arduino: Comparaison de deux dates"
programming_language: "Arduino"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/arduino/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Pourquoi

Lorsque vous programmez avec Arduino, il est parfois nécessaire de comparer deux dates. Cela peut être utile pour différentes raisons, telles que la planification d'événements futurs ou la vérification de la validité d'une date saisie par l'utilisateur.

## Comment faire

Pour comparer deux dates dans Arduino, vous devrez utiliser la fonction `compare()` spécifique à la bibliothèque `DateTime.h`. Cette fonction prend deux objets `DateTime` en tant que paramètres et renvoie un résultat sous forme de nombre entier, indiquant si la première date est antérieure, égale ou postérieure à la deuxième date.

Voici un exemple de code qui compare deux dates et affiche le résultat :

```Arduino
#include <DateTime.h>

void setup() {
  DateTime date1(2000, 1, 1, 0, 0, 0); // Crée un objet DateTime pour la première date
  DateTime date2(2020, 12, 31, 23, 59, 59); // Crée un objet DateTime pour la deuxième date

  int result = compare(date1, date2); // Compare les deux dates et enregistre le résultat dans une variable

  Serial.begin(9600); // Initialise la communication série
  Serial.print("Le résultat de la comparaison est : ");
  Serial.println(result); // Affiche le résultat dans le moniteur série
}

void loop() {
  // Code en boucle, n'est pas nécessaire pour cet exemple
}
```

Output :

```
Le résultat de la comparaison est : -1
```

Dans cet exemple, nous avons utilisé la fonction `compare()` pour comparer la date du 1er janvier 2000 avec celle du 31 décembre 2020. Comme le résultat est -1, cela signifie que la première date est antérieure à la deuxième.

## Plongeon dans les détails

Il est important de noter que la fonction `compare()` utilise la méthode [Epoch Time](https://en.wikipedia.org/wiki/Unix_time) pour comparer les dates. Cela signifie que les dates sont converties en un nombre entier représentant le nombre de secondes écoulées depuis le 1er janvier 1970 à 00: 00: 00.

Il est également possible de comparer des dates avec un intervalle de précision plus large que les secondes. Par exemple, vous pouvez utiliser la fonction `compareYear()` pour comparer uniquement les années de deux dates.

## Voir aussi

- [Documentation officielle de la bibliothèque DateTime](https://github.com/PaulStoffregen/DateTime)
- [Guide pour comparer des dates en Arduino](https://randomnerdtutorials.com/how-to-compare-dates-arduino-yun-and-esp8266/)

---

Merci d'avoir lu cet article sur la comparaison de deux dates avec Arduino ! Nous espérons que cela vous sera utile dans vos projets futurs. N'hésitez pas à explorer davantage la bibliothèque DateTime pour d'autres fonctionnalités liées à la gestion des dates. À bientôt !