---
title:    "Arduino: Transformer une date en chaîne de caractères"
keywords: ["Arduino"]
---

{{< edit_this_page >}}

## Pourquoi

Si vous aimez travailler avec le langage de programmation Arduino, vous aurez probablement besoin de convertir des données en une forme qui soit plus facile à utiliser. Par exemple, si vous avez besoin d'afficher une date dans un format spécifique, comme "jj/mm/aaaa", vous devrez convertir la date en une chaîne de caractères. Dans ce blog, nous allons vous montrer comment faire cela en utilisant les fonctions de conversion de date de l'Arduino.

## Comment faire

Pour convertir une date en une chaîne de caractères en utilisant Arduino, vous aurez besoin de la bibliothèque RTClib. Si vous n'avez pas encore cette bibliothèque installée, vous pouvez le faire en suivant ces étapes :

1. Ouvrez l'IDE Arduino.

2. Cliquez sur Sketch dans la barre de menu, puis sur Include Library, et enfin sur Manage Libraries.

3. Dans la barre de recherche, tapez "RTCLib" et appuyez sur Entrée.

4. Cliquez sur la bibliothèque RTClib et sur Install.

Maintenant que vous avez la bibliothèque installée, vous pouvez procéder à la conversion de la date en utilisant le code suivant :

```Arduino
#include <RTClib.h> // Importe la bibliothèque

RTC_DS1307 rtc; // Crée une instance de l'objet RTC

void setup() {
  Serial.begin(9600); // Initialise la communication série à une vitesse de 9600 bauds
  rtc.begin(); // Initialise la bibliothèque RTClib
}

void loop() {
  DateTime now = rtc.now(); // Enregistre la date et l'heure actuelles dans l'objet "now"
  String date = String(now.day()) + "/" + String(now.month()) + "/" + String(now.year()); // Convertit la date en une chaîne de caractères avec le format souhaité
  Serial.println(date); // Affiche la date dans la fenêtre du moniteur série
}
```

Exemple de sortie :

```
9/11/2021
```

## Plongée approfondie

Maintenant, voyons un peu plus en détail comment fonctionne la conversion de la date en une chaîne de caractères. Dans le code ci-dessus, nous utilisons la fonction `now()` de la bibliothèque RTClib pour obtenir la date et l'heure actuelles sous forme d'objet `DateTime`. Cet objet contient différentes fonctions pour accéder aux différentes parties de la date et de l'heure, telles que `day()`, `month()` et `year()`.

Nous utilisons ensuite ces fonctions dans la ligne suivante pour créer une chaîne de caractères avec le format souhaité en utilisant la fonction `String()` pour convertir les valeurs en chaînes de caractères. Enfin, nous imprimons cette chaîne de caractères dans la fenêtre du moniteur série à l'aide de la fonction `println()`.

De cette façon, vous pouvez facilement obtenir la date dans le format de votre choix et l'utiliser dans votre projet Arduino.

## Voir aussi

- [Documentation officielle de la bibliothèque RTClib](https://github.com/adafruit/RTClib/blob/master/README.md)
- [Tutoriel vidéo sur la conversion de date en chaîne de caractères en utilisant Arduino](https://www.youtube.com/watch?v=Jvx-dxJ5h3Q)
- [Exemples de projets Arduino utilisant la bibliothèque RTClib](https://create.arduino.cc/projecthub/search?q=RTCLib&type=tutorial)