---
title:    "Arduino: Chaînage de chaînes de caractères"
keywords: ["Arduino"]
---

{{< edit_this_page >}}

## Pourquoi

Pourquoi quelqu'un s'engagerait à concaténer des chaînes de caractères en programmation Arduino ? La réponse est simple : c'est une méthode essentielle pour manipuler et afficher du texte. Que vous souhaitiez afficher des messages sur un écran LCD, réaliser un projet de communication série ou simplement stocker des données dans une variable, la concaténation de chaînes de caractères sera un outil utile à connaître.

## Comment faire

La concaténation de chaînes de caractères consiste à combiner plusieurs chaînes de caractères en une seule. Pour cela, il vous suffit d'utiliser l'opérateur "+" entre les différentes chaînes que vous souhaitez concaténer.

Voici un exemple de code en Arduino :

```Arduino
// Exemple de concaténation de chaînes de caractères
void setup() {
  // Déclaration des chaînes de caractères
  String message1 = "Bonjour";
  String message2 = "à tous !";

  // Concaténation
  String messageFinal = message1 + " " + message2;

  // Affichage du message final
  Serial.println(messageFinal);
}

void loop() {
  // La boucle loop() est vide car nous n'avons besoin que du setup()
}
```

Lorsque vous allez exécuter ce code, vous verrez s'afficher sur le moniteur série le message "Bonjour à tous !". En concaténant les deux chaînes de caractères "Bonjour" et "à tous !", nous avons créé une variable contenant le message final que nous avons ensuite affiché en utilisant la fonction `println()`.

## Plongée en profondeur

En réalité, la concaténation de chaînes de caractères en Arduino est légèrement différente de celle dans d'autres langages de programmation. En effet, en raison des limitations de mémoire de la carte Arduino, la concaténation est gérée par une bibliothèque spécifique appelée "String". Cette bibliothèque alloue dynamiquement de la mémoire à la variable contenant le message final, ce qui peut entraîner des problèmes de performances si elle est utilisée de manière répétée.

Il est également important de noter que la bibliothèque "String" n'est pas compatible avec certains types de données, tels que les nombres entiers ou les tableaux. Dans ces cas-là, il est préférable d'utiliser d'autres méthodes de concaténation disponibles, telles que `strcpy()` ou `snprintf()`.

Il est donc recommandé de bien comprendre le fonctionnement de la concaténation de chaînes de caractères en Arduino et de l'utiliser avec précaution pour éviter tout problème de mémoire.

## Voir aussi

- [Documentation officielle d'Arduino sur la concaténation](https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/concat/)
- [Tutoriel vidéo sur la concaténation de chaînes de caractères en Arduino](https://www.youtube.com/watch?v=905VK4gxcdw)
- [Guide de référence du langage Processing (utilisé dans Arduino) sur la gestion des chaînes de caractères](https://processing.org/reference/String.html)