---
title:                "Arduino: Lecture des arguments de ligne de commande"
programming_language: "Arduino"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/arduino/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

# Pourquoi lire des arguments de ligne de commande en Arduino ?

Si vous développez des projets Arduino, vous êtes probablement familier avec l'utilisation de la console série pour lire des données provenant de capteurs ou pour afficher des résultats. Mais saviez-vous qu'il est également possible de lire des informations à partir de la ligne de commande de votre ordinateur ? Dans cet article, nous allons discuter de pourquoi vous pourriez vouloir utiliser cette fonctionnalité et comment le faire en utilisant l'Arduino IDE.

## Comment faire

Tout d'abord, il est important de comprendre ce qu'est une ligne de commande. Il s'agit d'un moyen de communiquer avec un ordinateur en tapant des instructions dans une fenêtre de terminal. En utilisant la ligne de commande, vous pouvez exécuter des programmes, modifier des fichiers, ou même lire des données sur l'Arduino.

Pour lire des commandes depuis la ligne de commande en Arduino, vous aurez besoin de trois choses : un câble pour connecter votre Arduino à votre ordinateur, un port série configuré dans l'IDE Arduino et un programme qui lira les données de la ligne de commande.

Pour commencer, connectez votre Arduino à votre ordinateur en utilisant un câble USB. Assurez-vous que le port série est correctement sélectionné dans l'IDE Arduino en allant dans Outils > Port série. Maintenant, nous pouvons écrire notre programme pour lire les commandes.

```Arduino
void setup(){
	Serial.begin(9600); // ouvrir le port série à une vitesse de 9600 bauds
}

void loop(){
	if (Serial.available()){ // vérifier si des données sont disponibles sur le port série
		String cmd = Serial.readStringUntil('\n'); // lire les données jusqu'au prochain saut de ligne
		// faire quelque chose avec la commande lue
	}
}
```

Le programme commence par initialiser le port série, puis entre dans une boucle qui vérifie constamment si des données sont disponibles. Si c'est le cas, il lit les données jusqu'au prochain saut de ligne et les stocke dans une chaîne de caractères. Vous pouvez ensuite utiliser cette chaîne pour exécuter des actions ou affecter des valeurs à des variables.

Essayez d'envoyer des commandes à partir de la ligne de commande en utilisant des valeurs numériques ou des chaînes de caractères pour voir comment le programme les traite.

## Profondeur

Maintenant que nous savons comment lire des commandes depuis la ligne de commande, voyons à quoi cela peut être utile. Tout d'abord, cela peut être une alternative pratique à la création d'une interface utilisateur pour votre projet. Plutôt que d'avoir à appuyer sur des boutons ou de toucher un écran, vous pouvez contrôler votre Arduino directement depuis votre ordinateur en tapant des commandes.

De plus, cela peut simplifier le processus de débogage en vous permettant de vérifier rapidement les valeurs de vos variables ou de tester différentes configurations sans avoir à téléverser un nouveau programme sur l'Arduino à chaque fois. Vous pouvez même utiliser la ligne de commande pour contrôler plusieurs Arduinos connectés à votre ordinateur en même temps.

Bien sûr, cela peut également avoir des applications plus avancées, selon vos besoins spécifiques. En utilisant les commandes de la ligne de commande, vous pouvez créer des programmes plus interactifs et même connecter votre Arduino avec des programmes ou des scripts fonctionnant sur votre ordinateur.

## Voir aussi

Pour en savoir plus sur la lecture des commandes de la ligne de commande en Arduino, consultez les ressources suivantes :

- [Documentation officielle Arduino pour le port série](https://www.arduino.cc/reference/en/language/functions/communication/serial/)
- [Un tutoriel sur comment utiliser la ligne de commande en Arduino](https://create.arduino.cc/projecthub/Arduino_Genuino/arduino-serial-basics-a74f94)
- [Un exemple de projet utilisant la ligne de commande pour contrôler un afficheur LED](https://www.arduino.cc/en/Tutorial/CommandLineModule)

Maintenant que vous savez comment lire des commandes depuis la ligne de commande en Arduino, vous pouvez explorer de nouvelles possibilités pour vos projets et améliorer votre expérience de développement. N'hésitez pas à expérimenter et à partager vos découvertes avec la communauté Arduino. Amusez-vous bien !