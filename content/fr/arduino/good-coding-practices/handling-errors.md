---
date: 2024-01-26 00:37:07.489429-07:00
description: "Comment faire : Disons que votre Arduino lit un capteur qui peut occasionnellement\
  \ produire des valeurs hors plage. Voici comment vous pourriez g\xE9rer cela\u2026"
lastmod: '2024-03-13T22:44:58.120849-06:00'
model: gpt-4-1106-preview
summary: Disons que votre Arduino lit un capteur qui peut occasionnellement produire
  des valeurs hors plage.
title: Gestion des erreurs
weight: 16
---

## Comment faire :
Disons que votre Arduino lit un capteur qui peut occasionnellement produire des valeurs hors plage. Voici comment vous pourriez gérer cela :

```Arduino
int sensorValue = analogRead(A0);

if (sensorValue >= 0 && sensorValue <= 1023) {
  // La valeur est dans la plage, continuer le traitement
  Serial.println(sensorValue);
} else {
  // La valeur est hors plage, gérer l'erreur
  Serial.println("Erreur : valeur du capteur hors plage.");
}
```
Sortie d'exemple :
```
523
Erreur : valeur du capteur hors plage.
761
```

## Approfondissement
La gestion des erreurs n'a pas toujours été aussi simple. Aux premiers jours, les développeurs ignoraient souvent les erreurs, menant au redouté "comportement indéfini". Au fur et à mesure que la programmation évoluait, les outils aussi — vous avez maintenant des exceptions dans de nombreux langages, mais il reste un vieux réflexe de 'vérifie-d'abord' dans le monde Arduino en raison des contraintes matérielles et des racines en C++.

Dans la programmation Arduino, on voit souvent des déclarations `if-else` pour la gestion des erreurs. Mais il existe des alternatives : utiliser la fonction `assert` pour arrêter l'exécution si une condition échoue, ou concevoir des sécurités dans votre configuration matérielle elle-même.

Lors de la mise en œuvre de la gestion des erreurs, considérez l'impact de l'arrêt du programme par rapport à la possibilité de le continuer avec un état par défaut ou sûr. C'est un compromis, et le bon choix dépend du risque de dommages causés par les interruptions par rapport à une opération incorrecte.

## Voir Aussi
Perfectionnez-vous sur la détection et la gestion des erreurs avec ces liens :

- Référence du langage Arduino : https://www.arduino.cc/reference/en/
- L'analyse approfondie de la gestion des erreurs par Embedded Artistry : https://embeddedartistry.com/blog/2017/05/17/creating-a-circular-buffer-in-c-and-c/
- Gestion des erreurs en C++ : https://en.cppreference.com/w/cpp/error/exception

Cela devrait vous donner les connaissances et la confiance nécessaires pour éviter les pièges des erreurs dans vos aventures avec Arduino.
