---
title:                "Arduino: Convertir une date en chaîne de caractères"
simple_title:         "Convertir une date en chaîne de caractères"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/arduino/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

# Pourquoi Convertir une Date en Chaîne de Caractères en Arduino

Si vous êtes un passionné d'Arduino, vous savez probablement déjà que ce microcontrôleur peut être utilisé pour réaliser une variété de projets passionnants. L'une des fonctionnalités les plus utiles de l'Arduino est sa capacité à enregistrer et traiter des dates. Mais parfois, il peut être nécessaire de convertir cette date en une chaîne de caractères, c'est-à-dire un ensemble de caractères en texte. Dans cet article, nous allons expliquer pourquoi vous pourriez avoir besoin de convertir une date en une chaîne de caractères et comment le faire en utilisant Arduino.

## Comment Convertir une Date en Chaîne de Caractères en Arduino

Pour convertir une date en une chaîne de caractères, vous devez utiliser la fonction `sprintf()` en Arduino. Cette fonction prend trois arguments : le premier est une chaîne de caractères qui spécifie le format dans lequel vous souhaitez que la date soit convertie, le deuxième est une variable qui stocke la date en tant que `Struct tm` et le troisième est la chaîne de caractères résultante.

```Arduino
#include <Time.h>

// Variable pour stocker la date et l'heure actuelles
tmElements_t date;

// Récupérer la date actuelle
RTC.read(date);

// Convertir la date en une chaîne de caractères
char dateString[25];
sprintf(dateString, "%02d/%02d/%04d", date.Day, date.Month, date.Year + 1970);
```

Dans cet exemple, nous utilisons le format "jour/mois/année" pour convertir la date en une chaîne de caractères. Vous pouvez choisir le format qui convient le mieux à votre projet en utilisant les spécificateurs de format disponibles pour la fonction `sprintf()`.

## Plongeon en Profondeur

Si vous êtes curieux de connaître le fonctionnement interne de la conversion de date en chaîne de caractères en Arduino, voici quelques informations supplémentaires pour vous satisfaire. La fonction `sprintf()` utilise les informations de `Struct tm` pour extraire chaque élément de la date (jour, mois, année) et les ajoute à une chaîne de caractères en respectant le format spécifié.

De plus, il est important de noter que la prise en charge des dates en Arduino est basée sur le temps Unix, qui représente le nombre de secondes écoulées depuis le 1er janvier 1970. Cela explique pourquoi nous devons ajouter 1970 pour obtenir l'année correcte dans notre exemple.

## See Also

Si vous souhaitez en savoir plus sur la manipulation des dates en Arduino, voici quelques liens utiles :

- [Documentation Arduino pour la fonction `sprintf()`](https://www.arduino.cc/reference/en/language/functions/character-functions/sprintf/)
- [Tutoriel sur la gestion du temps en Arduino](https://learn.adafruit.com/memories-of-an-arduino/coding-for-use-with-the-real-time-clock)

Nous espérons que cet article vous a aidé à comprendre comment convertir une date en une chaîne de caractères en utilisant Arduino. Amusez-vous bien dans vos projets et n'hésitez pas à explorer davantage les fonctionnalités de ce microcontrôleur polyvalent.