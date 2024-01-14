---
title:                "Arduino: Transformer une date en chaîne de caractères"
programming_language: "Arduino"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/arduino/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Pourquoi

La conversion d'une date en une chaîne de caractères est une tâche courante lors de la programmation en Arduino. Cela peut être utile pour l'affichage de la date et de l'heure actuelles sur un écran LCD ou pour sauvegarder des données dans une carte SD dans un format facilement identifiable. Dans cet article, nous allons explorer comment réaliser cette conversion en utilisant Arduino.

## Comment faire

Pour convertir une date en une chaîne de caractères, nous allons utiliser la bibliothèque Time d'Arduino. Cette bibliothèque permet de manipuler facilement les informations liées au temps comme la date, l'heure et les fuseaux horaires. Tout d'abord, nous devons inclure la bibliothèque Time dans notre code :

```Arduino
#include <Time.h>
```

Ensuite, nous pouvons utiliser la fonction `format()` pour convertir une date en une chaîne de caractères, en spécifiant le format souhaité. Par exemple, pour convertir la date actuelle en une chaîne de caractères au format dd/mm/yyyy, nous utiliserons la ligne suivante dans notre code :

```Arduino
String date = String(format(Time.now(), "%d/%m/%Y"));
```

La variable `date` contiendra maintenant la date au format souhaité. Nous pouvons également ajouter l'heure à cette chaîne en utilisant la même méthode :

```Arduino
String dateHeure = String(format(Time.now(), "%d/%m/%Y %H:%M:%S"));
```

La variable `dateHeure` contiendra maintenant la date et l'heure actuelles au format dd/mm/yyyy hh:mm:ss. Vous pouvez expérimenter avec différents formats en utilisant la fonction `format()` pour obtenir le résultat souhaité.

## Plongée en profondeur

La bibliothèque Time d'Arduino utilise un objet de type `tmElements_t` pour stocker les informations liées au temps. Cet objet contient différentes variables telles que `tmYear`, `tmMonth` et `tmDay` pour la date et `tmHour`, `tmMinute` et `tmSecond` pour l'heure. La fonction `format()` prend cet objet en entrée et retourne une chaîne de caractères en fonction du format spécifié. Vous pouvez également utiliser la fonction `makeTime()` pour créer un objet `tmElements_t` à partir de valeurs spécifiques, ce qui peut s'avérer utile si vous devez travailler avec une date et une heure personnalisées.

## Voir aussi

- [Documentation de la bibliothèque Time d'Arduino](https://www.arduino.cc/en/Reference/Time)
- [Tutoriel vidéo sur la conversion d'une date en une chaîne de caractères en utilisant Arduino](https://www.youtube.com/watch?v=7IQhNOs5W5M)
- [Forum Arduino dédié à la bibliothèque Time](https://forum.arduino.cc/index.php?topic=239078.0)