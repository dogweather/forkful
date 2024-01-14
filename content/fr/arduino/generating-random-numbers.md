---
title:                "Arduino: Générer des nombres aléatoires"
programming_language: "Arduino"
category:             "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/arduino/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Pourquoi

La génération de nombres aléatoires peut sembler être une tâche inutile ou sans intérêt, mais c'est en fait une pratique très utile en programmation. En utilisant des nombres aléatoires, vous pouvez ajouter une certaine dose d'imprévisibilité à vos projets, ce qui peut les rendre plus excitants et plus intéressants.

## Comment faire

Générer des nombres aléatoires avec Arduino est assez simple. Voici un exemple de code et sa sortie :

```Arduino
// Définition de la broche utilisée pour la génération de nombres aléatoires
int broche = A0;

// Initialisation de la broche en tant qu'entrée
pinMode(broche, INPUT);

// Génération d'un nombre aléatoire entre 0 et 1023
int nombreAleatoire = analogRead(broche);

// Impression du résultat
Serial.println(nombreAleatoire);
```

La sortie pourrait ressembler à ceci : `472`

Vous pouvez également générer des nombres aléatoires dans une plage spécifique en utilisant la fonction `random(min, max)`. Par exemple, si vous voulez générer un nombre aléatoire entre 1 et 10, vous pouvez utiliser le code suivant :

```Arduino
// Génération d'un nombre aléatoire entre 1 et 10
int nombreAleatoire = random(1, 11);
```

## Plongée profonde

La méthode utilisée dans l'exemple ci-dessus pour générer des nombres aléatoires n'est pas vraiment aléatoire. En fait, elle utilise des mesures de bruit dans le circuit pour produire une séquence apparemment aléatoire de nombres. Si vous voulez générer des nombres vraiment aléatoires, vous pouvez utiliser un composant externe, tel qu'un module de génération de nombres aléatoires basé sur le bruit thermique ou sur des données environnementales.

Il est également important de noter que la séquence de nombres générée par Arduino est pseudo-aléatoire, ce qui signifie qu'elle peut être reproduite si les mêmes conditions sont créées à nouveau. Si vous voulez une séquence toujours différente, vous pouvez utiliser un générateur de nombres aléatoires externe ou utiliser une graine aléatoire (seed) différente à chaque exécution de votre code.

## Voir aussi

- Tutoriel complet sur la génération de nombres aléatoires avec Arduino : [https://www.arduino.cc/reference/en/language/functions/random-numbers/random/](https://www.arduino.cc/reference/en/language/functions/random-numbers/random/)
- Informations sur les différents types de générateurs de nombres aléatoires : [https://en.wikipedia.org/wiki/Random_number_generation](https://en.wikipedia.org/wiki/Random_number_generation)
- Exemple de projet utilisant des nombres aléatoires pour créer un jeu de hasard avec Arduino : [https://create.arduino.cc/projecthub/JanCuba/very-simple-slot-machine-with-arduino-cef794](https://create.arduino.cc/projecthub/JanCuba/very-simple-slot-machine-with-arduino-cef794)