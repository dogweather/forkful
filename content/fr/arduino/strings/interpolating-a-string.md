---
aliases:
- /fr/arduino/interpolating-a-string/
date: 2024-01-20 17:50:11.539298-07:00
description: "L'interpolation de cha\xEEnes permet d'ins\xE9rer des valeurs de variables\
  \ directement dans une cha\xEEne de caract\xE8res. Les programmeurs l'utilisent\
  \ pour\u2026"
lastmod: 2024-02-18 23:09:09.099922
model: gpt-4-1106-preview
summary: "L'interpolation de cha\xEEnes permet d'ins\xE9rer des valeurs de variables\
  \ directement dans une cha\xEEne de caract\xE8res. Les programmeurs l'utilisent\
  \ pour\u2026"
title: "Interpolation de cha\xEEnes de caract\xE8res"
---

{{< edit_this_page >}}

## What & Why?
L'interpolation de chaînes permet d'insérer des valeurs de variables directement dans une chaîne de caractères. Les programmeurs l'utilisent pour assembler du texte dynamique de manière lisible et efficace.

## How to:
L'Arduino n'a pas d'interpolation de chaînes intégrée comme dans d'autres langages, mais on peut concaténer et utiliser `sprintf`. Voici un exemple :

```Arduino
char buffer[50];
int sensorValue = analogRead(A0);
float temperature = sensorValue / 10.0;

sprintf(buffer, "Valeur: %d, Temp: %.1f", sensorValue, temperature);
Serial.println(buffer);
```

Sortie :
```
Valeur: 402, Temp: 40.2
```

## Deep Dive
Historiquement, les microcontrôleurs avaient peu de mémoire, donc les fonctions comme `sprintf` étaient évitées. Aujourd'hui, avec plus de RAM, on les utilise pour leur simplicité, malgré leur gourmandise en ressources.

Dans d'autres contextes, on pourrait utiliser des méthodes d'interpolation de chaînes plus avancées, comme la classe `String` en Arduino :

```Arduino
String message = "Valeur: " + String(sensorValue) + ", Temp: " + String(temperature);
Serial.println(message);
```

Cependant, les objets `String` peuvent fragmenter la mémoire, donc leur usage est parfois déconseillé.

Pour une utilisation mémoire optimale, on peut assembler manuellement les chaînes en manipulant les tableaux de caractères.

## See Also
- Documentation Arduino `sprintf`: https://www.arduino.cc/reference/en/language/functions/characters/characterclassification/sprintf/
- Un avertissement sur l'utilisation de la classe `String`: https://www.arduino.cc/reference/en/language/variables/data-types/stringobject/
- Forum Arduino pour des astuces et discussions: https://forum.arduino.cc/
