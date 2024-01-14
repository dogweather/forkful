---
title:    "Arduino: Obtenir la date actuelle"
keywords: ["Arduino"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/arduino/getting-the-current-date.md"
---

{{< edit_this_page >}}

# Pourquoi
Avant de plonger dans le code, il est important de comprendre pourquoi quelqu'un voudrait obtenir la date actuelle à travers l'Arduino. Cela est particulièrement utile pour les projets qui nécessitent une synchronisation avec le calendrier, comme un compteur de jours, un programme d'arrosage automatique ou une horloge.

# Comment Faire
Pour obtenir la date actuelle à travers l'Arduino, il est important de comprendre comment fonctionnent les horloges en temps réel (RTC). Vous aurez besoin d'un module RTC connecté à votre Arduino, qui utilisera une pile pour maintenir l'heure et la date même lorsque l'alimentation est coupée. 

Ensuite, vous devrez installer une bibliothèque RTC dans votre IDE Arduino en allant dans `Sketch > Bibliothèque > Gérer les bibliothèques` et en recherchant "RTC". Une fois la bibliothèque installée, vous pouvez suivre les étapes suivantes pour obtenir la date actuelle dans votre code :

```Arduino
#include <RTClib.h>  //Inclure la bibliothèque RTC
RTC_DS1307 rtc;      //Déclarer l'objet RTC

void setup() {
  Serial.begin(9600);  //Initialiser la communication série pour l'affichage du résultat 
  rtc.begin();         //Initialiser l'objet RTC
  if (! rtc.isrunning()) { //Vérifier si l'horloge est en marche, sinon la régler
    rtc.adjust(DateTime(F(__DATE__), F(__TIME__)));
  }
}

void loop() {
  DateTime now = rtc.now();  //Obtenir la date et l'heure actuelles
  Serial.print(now.year());  //Imprimer l'année sur le moniteur série
  Serial.print("/");         //Imprimer le séparateur de dates "/"
  Serial.print(now.month()); //Imprimer le mois
  Serial.print("/");         
  Serial.print(now.day());   //Imprimer le jour
  Serial.println();          //Retour à la ligne
  delay(1000);               //Attendre une seconde avant de refaire la lecture
}
```

En téléversant ce code dans votre Arduino et en ouvrant le moniteur série, vous devriez voir la date actuelle s'afficher en continu.

# Plongeons plus Profondément
Il est important de noter que les dates et les heures sont stockées sous forme de nombres dans l'objet `DateTime` de la bibliothèque RTC. Vous pouvez utiliser ces nombres pour effectuer des calculs ou des comparaisons, mais vous devrez vous référer à la documentation de la bibliothèque pour comprendre la signification de chaque nombre.

De plus, il est important de maintenir l'horloge RTC à jour en ajustant son heure et sa date lorsque le code est téléversé ou lorsqu'il y a une perte de puissance. Cela peut être fait en utilisant la fonction `rtc.adjust()` dans la section `setup()` comme dans l'exemple ci-dessus.

Enfin, il est également possible de configurer l'horloge RTC pour qu'elle utilise des fuseaux horaires différents en utilisant la fonction `rtc.adjust()` avec les paramètres appropriés.

# Voir Aussi
- Tutoriel vidéo sur l'utilisation d'un module RTC avec l'Arduino : https://www.youtube.com/watch?v=i4ibR0JOFoc
- Documentation complète de la bibliothèque RTClib : https://github.com/adafruit/RTClib
- Projet d'horloge en temps réel avec l'Arduino : https://create.arduino.cc/projecthub/arjun/realtime-clock-with-ds1307-a4d878
- Exemple de code pour convertir un objet `DateTime` en une chaîne de caractères : https://github.com/arduino-libraries/RTClib/issues/4