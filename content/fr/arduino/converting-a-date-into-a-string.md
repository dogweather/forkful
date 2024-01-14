---
title:    "Arduino: Transformer une date en chaîne de caractères"
keywords: ["Arduino"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/arduino/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Pourquoi

Vous êtes peut-être en train de programmer votre propre horloge ou calendrier sur votre Arduino, ou vous avez simplement besoin de présenter la date sous forme de texte. Dans les deux cas, vous aurez besoin de convertir la date en une chaîne de caractères (string). Heureusement, c'est un processus simple et nous allons vous montrer comment le faire dans cet article.

## Comment faire

Pour convertir une date en une chaîne de caractères, nous allons utiliser la fonction `sprintf()` de la bibliothèque `Time.h`. Voici un exemple de code :

```Arduino
#include <Time.h>

void setup() {
  Serial.begin(9600); // initialiser la communication série
  setTime(12, 34, 56, 1, 4, 2021); // définir la date et l'heure actuelles
}

void loop() {
  char dateStr[11]; // déclarer un tableau pour stocker la date
  sprintf(dateStr, "%02d/%02d/%04d", day(), month(), year()); // utiliser sprintf pour convertir la date en une chaîne
  Serial.println(dateStr); // afficher la date sur le moniteur série
  delay(1000);
}
```

Dans cet exemple, nous utilisons la fonction `setTime()` pour définir une date spécifique. Ensuite, nous déclarons un tableau de char (`char`) pour stocker la date sous forme de chaîne avec une taille de 11 caractères (ce qui est suffisant pour une date au format JJ/MM/AAAA). Enfin, nous utilisons `sprintf()` pour remplir ce tableau avec la date formatée en utilisant les fonctions `day()`, `month()` et `year()` fournies par la bibliothèque `Time.h`.

Lorsque le code est exécuté, vous devriez obtenir une sortie telle que `04/01/2021` sur le moniteur série de votre Arduino.

## Plongez plus profondément

La fonction `sprintf()` est un moyen pratique de convertir des nombres en chaînes de caractères en utilisant un format spécifique, comme nous l'avons fait avec la date dans l'exemple précédent. Cependant, il existe d'autres moyens de convertir des nombres en chaînes, comme la fonction `itoa()` qui convertit un entier en chaîne ou la fonction `String()` qui crée un objet String à partir d'un entier ou d'un nombre à virgule flottante.

Il est également intéressant de noter que la bibliothèque `Time.h` fournit d'autres fonctions pratiques pour obtenir la date et l'heure actuelles, ainsi que pour effectuer des opérations sur celles-ci.

## Voir aussi

- La documentation officielle de la bibliothèque `Time.h` : https://www.arduino.cc/en/Reference/Time
- Un tutoriel sur l'utilisation des fonctions `sprintf()` et `itoa()` : https://www.tutorialspoint.com/c_standard_library/c_function_sprintf.htm