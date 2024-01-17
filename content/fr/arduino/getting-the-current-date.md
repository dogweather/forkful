---
title:                "Obtenir la date actuelle"
html_title:           "Arduino: Obtenir la date actuelle"
simple_title:         "Obtenir la date actuelle"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/arduino/getting-the-current-date.md"
---

{{< edit_this_page >}}

# Obtenir la Date Actuelle dans Arduino

## Qu'est-ce que c'est et pourquoi le faire ?

Obtenir la date actuelle dans Arduino, c'est obtenir la date et l'heure exactes à l'instant présent. Les programmeurs font cela afin de pouvoir enregistrer des données avec une précision temporelle et de synchroniser des événements dans leur code.

## Comment faire :

Pour obtenir la date actuelle dans Arduino, on peut utiliser la fonction ```now()```. Cette fonction renvoie un objet de type ```DateTime``` avec l'heure et la date actuelles. 

Exemple de code :

```Arduino
DateTime now = now();
Serial.println(now.year(), DEC);
Serial.println(now.month(), DEC);
Serial.println(now.day(), DEC);
Serial.println(now.hour(), DEC);
Serial.println(now.minute(), DEC);
Serial.println(now.second(), DEC);
```

Résultat :

```Arduino
2021
10
24
9
30
15
```

## Plongée en Profondeur :

Historiquement, pour obtenir la date actuelle, les programmeurs Arduino utilisent la bibliothèque TimeLib, qui fonctionne avec la fonction ```now()```. Cependant, il existe des alternatives telles que la bibliothèque Time de Paul Stoffregen et la bibliothèque DateTime de Makuna.

L'implémentation de la fonction ```now()``` dans Arduino utilise une horloge en temps réel (RTC) pour maintenir la date et l'heure précises. La plupart des cartes Arduino n'en ont pas intégré, il est donc nécessaire d'en ajouter une séparément.

## À Voir Aussi :

- Documentation officielle d'Arduino sur la fonction ```now()``` : https://www.arduino.cc/reference/en/libraries/datetime/
- Bibliothèque Time de Paul Stoffregen : https://www.pjrc.com/teensy/td_libs_Time.html
- Bibliothèque DateTime de Makuna : https://github.com/Makuna/Rtc/wiki