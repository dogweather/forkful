---
title:                "Arduino: Lecture des arguments de ligne de commande"
simple_title:         "Lecture des arguments de ligne de commande"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/arduino/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Pourquoi 

Si vous êtes nouveau dans le monde de la programmation Arduino, vous vous demandez peut-être pourquoi vous devriez apprendre à lire les arguments de ligne de commande. Cela peut sembler intimidant, mais c'est en fait une compétence utile qui vous permettra de personnaliser et de contrôler votre programme en lui donnant des instructions spécifiques à chaque exécution. Dans cet article, nous allons vous expliquer comment lire les arguments de ligne de commande et comment cela peut être utile pour vos projets Arduino.

## Comment faire

La première étape pour lire les arguments de ligne de commande est de les inclure dans votre code Arduino. Pour ce faire, vous devez utiliser la fonction ```Arduino.start```. Par exemple, si vous avez besoin de lire un argument numérique, vous pouvez utiliser la fonction ```parseInt()``` pour le convertir en un nombre. Voici un exemple de code pour lire un argument de ligne de commande et l'afficher sur le moniteur série :

```
Arduino.start(argc, argv);
int num = parseInt(argv[1]);
Serial.println("L'argument est : ");
Serial.println(num);
```

Ensuite, vous pouvez envoyer cet argument depuis votre ordinateur vers votre carte Arduino en utilisant l'invite de commande ou le terminal. Par exemple, si vous utilisez l'invite de commande de Windows, vous pouvez taper la commande suivante :

```
monprogramme.exe 123
```

Cela enverra la valeur numérique 123 en tant qu'argument à votre programme Arduino, qui sera alors lu et affiché sur le moniteur série.

## Deep Dive

Maintenant que vous savez comment lire les arguments de ligne de commande, voici quelques informations supplémentaires sur leur fonctionnement. Les arguments de ligne de commande peuvent être utiles si vous ne voulez pas modifier en permanence votre code Arduino pour chaque exécution ou si vous souhaitez que votre programme fonctionne de manière dynamique en fonction des valeurs d'entrée. De plus, vous pouvez également utiliser des arguments de ligne de commande pour déboguer votre programme en affichant des valeurs spécifiques au lieu de les définir dans votre code.

Il est important de noter que la lecture des arguments de ligne de commande ne fonctionne pas sur toutes les cartes Arduino. En général, cela fonctionne mieux avec les cartes basées sur un processeur AVR, telles que les cartes Uno et Mega. De plus, vous pouvez également utiliser des bibliothèques tierces pour simplifier la lecture des arguments de ligne de commande et ajouter des fonctionnalités supplémentaires.

## Voir aussi

Maintenant que vous savez comment lire les arguments de ligne de commande, voici quelques liens utiles pour en apprendre davantage sur la programmation Arduino :

- Apprenez à utiliser la fonction parseInt() dans la [documentation Arduino](https://www.arduino.cc/reference/en/language/functions/conversion/parseint/).
- Découvrez comment utiliser des bibliothèques tierces pour gérer les arguments de ligne de commande avec [le guide de SparkFun](https://learn.sparkfun.com/tutorials/command-line-arguments-explained).
- Améliorez vos compétences en programmation Arduino avec [les tutoriels de OpenClassrooms](https://openclassrooms.com/fr/courses/19980-apprenez-a-programmer-en-c-sur-arduino).