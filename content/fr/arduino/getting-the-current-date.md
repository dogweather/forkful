---
title:    "Arduino: Obtenir la date actuelle"
keywords: ["Arduino"]
---

{{< edit_this_page >}}

# Pourquoi

Il est essentiel de connaître la date actuelle pour de nombreuses applications telles que la gestion de données, la création de calendriers ou l'enregistrement de données chronologiques. Dans cet article, je vais vous montrer comment obtenir facilement la date actuelle grâce à la programmation Arduino.

# Comment Faire 

Pour obtenir la date actuelle sur Arduino, nous allons utiliser la bibliothèque intégrée "Time.h". Tout d'abord, nous devons l'inclure dans notre code avec la ligne suivante : 

```Arduino
#include <Time.h>
```

Ensuite, nous devons initialiser la bibliothèque en utilisant la fonction "setTime()", en spécifiant l'heure, la date et le fuseau horaire souhaités. Voici un exemple :

```Arduino
setTime(12, 0, 0, 1, 1, 2020); // Définir l'heure à midi, le 1er janvier 2020
```

Ensuite, pour obtenir la date actuelle, nous allons utiliser la fonction "time()" qui renvoie le nombre de secondes écoulées depuis minuit le 1er janvier 1970. Nous pouvons ensuite utiliser cette valeur pour calculer la date actuelle avec les fonctions fournies par la bibliothèque "Time.h". Voici un exemple : 

```Arduino
time_t t = time(NULL); // Récupérer le temps actuel
tm *now = localtime(&t); // Transformer en format de date
int year = now->tm_year + 1900; // Obtenir l'année
int month = now->tm_mon + 1; // Obtenir le mois
int day = now->tm_mday; // Obtenir le jour
```

Nous pouvons également afficher cette date dans le moniteur série en utilisant la fonction "Serial.println()". Voici un exemple complet : 

```Arduino
#include <Time.h>

void setup() {
  setTime(12, 0, 0, 1, 1, 2020); // Initialiser la bibliothèque
  Serial.begin(9600); // Démarrer la communication avec le moniteur série
}

void loop() {
  time_t t = time(NULL); // Récupérer le temps actuel
  tm *now = localtime(&t); // Transformer en format de date
  int year = now->tm_year + 1900; // Obtenir l'année
  int month = now->tm_mon + 1; // Obtenir le mois
  int day = now->tm_mday; // Obtenir le jour

  Serial.print("La date est : "); // Afficher la date dans le moniteur série
  Serial.print(day);
  Serial.print("/");
  Serial.print(month);
  Serial.print("/");
  Serial.println(year);
  delay(1000); // Attendre une seconde avant de répéter
}
```

Lorsque vous téléversez ce code sur votre carte Arduino et ouvrez le moniteur série, vous devriez voir la date actuelle s'afficher en continu.

# Analyse Approfondie

La bibliothèque "Time.h" utilise le temps universel coordonné (UTC) pour calculer la date et l'heure. Cela signifie que les valeurs retournées peuvent ne pas correspondre exactement à l'heure locale de l'utilisateur en fonction de son emplacement géographique et du changement d'heure. Pour éviter cela, la bibliothèque propose des fonctions pour convertir l'heure locale en UTC et vice-versa.

De plus, en utilisant la fonction "now()->tm_wday", nous pouvons obtenir le jour de la semaine en commençant par 0 pour le dimanche, 1 pour le lundi, etc. La bibliothèque propose également d'autres fonctions pour formater la date et l'heure selon vos préférences.

# Voir Aussi

- Documentation officielle de la bibliothèque "Time.h" : https://www.arduino.cc/en/Reference/DateTime
- Tutoriel sur l'utilisation de la bibliothèque "Time.h" : https://randomnerdtutorials.com/guide-getting-date-time-arduino/