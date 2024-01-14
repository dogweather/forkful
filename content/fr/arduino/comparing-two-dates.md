---
title:    "Arduino: Comparer deux dates"
keywords: ["Arduino"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/arduino/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Pourquoi

Imaginez que vous souhaitiez créer une horloge intelligente avec votre carte Arduino. Vous aurez besoin de comparer deux dates pour savoir si l'heure actuelle est antérieure ou postérieure à une date spécifique comme votre anniversaire, par exemple. Comprendre comment comparer deux dates est donc essentiel pour réaliser ce projet.

## Comment Faire

Pour comparer deux dates, vous devrez utiliser des opérateurs de comparaison tels que ">", "<" ou "==". Voici un exemple de code Arduino pour comparer deux dates:

````Arduino
// Définition des variables pour les deux dates à comparer
int jour1 = 1;
int mois1 = 1;
int annee1 = 2020;

int jour2 = 31;
int mois2 = 12;
int annee2 = 2019;

// Comparaison des dates
if (annee1 > annee2) {
    // code à exécuter si la première date est plus récente que la deuxième
    Serial.println("La première date est plus récente que la deuxième !");
} else if (annee1 < annee2) {
    // code à exécuter si la première date est plus ancienne que la deuxième
    Serial.println("La première date est plus ancienne que la deuxième !");
} else if (mois1 > mois2) {
    // code à exécuter si les années sont égales mais que le mois de la première date est plus récent que celui de la deuxième
    Serial.println("La première date est plus récente que la deuxième !");
} else if (mois1 < mois2) {
    // code à exécuter si les années sont égales mais que le mois de la première date est plus ancien que celui de la deuxième
    Serial.println("La première date est plus ancienne que la deuxième !");
} else if (jour1 > jour2) {
    // code à exécuter si les années et les mois sont égaux mais que le jour de la première date est plus récent que celui de la deuxième
    Serial.println("La première date est plus récente que la deuxième !");
} else if (jour1 < jour2) {
    // code à exécuter si les années et les mois sont égaux mais que le jour de la première date est plus ancien que celui de la deuxième
    Serial.println("La première date est plus ancienne que la deuxième !");
} else {
    // code à exécuter si les deux dates sont identiques
    Serial.println("Les deux dates sont identiques !");
}
````

La sortie de ce code sera "La première date est plus ancienne que la deuxième !", car l'année de la première date est inférieure à celle de la deuxième. Essayez de modifier les valeurs des variables pour voir comment la sortie change en conséquence.

## Plongée en profondeur

La comparaison de dates peut sembler simple, mais il y a des choses à prendre en compte pour éviter des erreurs. Par exemple, il est important de garder à l'esprit les années bissextiles et les différentes longueurs de mois. Vous pouvez également utiliser des fonctions intégrées d'Arduino, telles que `millis()` pour trouver le nombre de millisecondes écoulées depuis le démarrage de votre carte et les convertir en dates. Avec ces connaissances, vous pouvez créer des projets plus avancés tels qu'un calendrier à affichage LED ou un système de rappel d'anniversaire.

## Voir aussi

- [Documentation sur les opérateurs de comparaison en C++](https://www.cplusplus.com/doc/tutorial/operators/)
- [Guide sur la gestion du temps avec Arduino](https://randomnerdtutorials.com/guide-for-real-time-clock-rtc-module-with-arduino/)
- [Exemples de projets avec la gestion de dates et le temps en Arduino](https://create.arduino.cc/projecthub/projects/tags/date-and-time)