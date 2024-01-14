---
title:    "Arduino: Calculer une date dans le futur ou le passé"
keywords: ["Arduino"]
---

{{< edit_this_page >}}

## Pourquoi

Nous avons souvent besoin de savoir quelle date sera dans le futur ou dans le passé à partir d'une date donnée. Peut-être pour planifier un événement ou gérer une tâche. Dans cet article, nous allons apprendre comment calculer une date dans le futur ou dans le passé en utilisant Arduino.

## Comment faire

Pour commencer, nous devons déterminer la date de référence à partir de laquelle nous allons calculer la date dans le futur ou dans le passé. Pour cela, nous utiliserons la fonction "Utiliser l'horloge en temps réel" de la bibliothèque Time pour Arduino.

Ensuite, définissons une variable pour représenter cette date de référence :

```Arduino
time_t dateRef = {20,0,0,5,2,2021};
  // Temps de référence : 05 février 2021 à 20h00m20s
``` 

Maintenant, pour calculer une date dans le futur, nous pouvons utiliser la fonction "Utiliser la date future" également de la bibliothèque Time. Cette fonction prend en compte le temps de référence et le nombre de secondes à ajouter à cette date pour obtenir la date dans le futur.

Voici un exemple avec une date future de 2 semaines après la date de référence :

```Arduino
time_t dateFuture = dateRef + 1209600; 
  // 1209600 secondes correspondent à 2 semaines (2 * 7 * 24 * 3600)
``` 

De la même manière, pour calculer une date dans le passé, nous pouvons utiliser la fonction "Utiliser la date précédente", en soustrayant cette fois-ci le nombre de secondes désiré :

```Arduino
time_t datePassé = dateRef - 86400; 
  // 86400 secondes correspondent à 1 jour (24 * 3600)
``` 

Et voilà, nous avons calculé une date dans le futur et une date dans le passé à partir d'une date de référence !

## Plongée en profondeur

Il est important de comprendre le fonctionnement de la bibliothèque Time pour pouvoir utiliser les fonctions que nous avons mentionnées. Cette bibliothèque utilise un format de temps UNIX, qui mesure le temps en secondes écoulées depuis le 1er janvier 1970. C'est pourquoi lors de l'utilisation de la fonction de calcul de date future ou précédente, nous utilisons un nombre de secondes en tant qu'argument.

De plus, la bibliothèque Time offre de nombreuses autres fonctions pour gérer le temps, comme la conversion de temps en millisecondes ou encore le décalage horaire. N'hésitez pas à explorer ces fonctionnalités pour en apprendre davantage sur la gestion du temps en programmation avec Arduino.

## Voir aussi

- Documentation de la bibliothèque Time pour Arduino : https://www.arduino.cc/reference/en/libraries/time/
- Tutoriel vidéo sur l'utilisation de la bibliothèque Time pour Arduino : https://www.youtube.com/watch?v=Hzormsxm_9I
- Exemple de projet utilisant la bibliothèque Time pour gérer les tâches planifiées : https://www.instructables.com/Arduino-Event-SchedulingSystem/