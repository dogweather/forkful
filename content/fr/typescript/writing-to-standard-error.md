---
title:                "Écrire vers la sortie d'erreur standard"
html_title:           "TypeScript: Écrire vers la sortie d'erreur standard"
simple_title:         "Écrire vers la sortie d'erreur standard"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/typescript/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Qu'est-ce que c'est et pourquoi?
Ecrire sur la sortie d'erreur standard (standard error output) est une pratique courante dans la programmation qui consiste à afficher des messages d'erreur lorsqu'un programme rencontre un problème. Cela permet aux programmeurs de déboguer et de résoudre les erreurs plus facilement.

## Comment procéder:
Ci-dessous, vous trouverez des exemples de code en ```TypeScript``` montrant comment écrire sur la sortie d'erreur standard et le résultat attendu :

**Exemple 1 :**
```
console.error("Erreur : Impossible de diviser par zéro");
```

**Résultat attendu :**
```
Erreur : Impossible de diviser par zéro
```

**Exemple 2 :**
```
try {
  // du code qui génère une erreur
} catch (error) {
  console.error(`Erreur : ${error.message}`);
}
```

**Résultat attendu :**
```
Erreur : Undefined is not a function
```

## Plongée en profondeur:
Ecrire sur la sortie d'erreur standard est une pratique qui remonte aux premiers jours de la programmation informatique. Avant l'utilisation généralisée des ordinateurs personnels, les programmeurs devaient utiliser des terminaux pour accéder aux ordinateurs centraux par le biais desquels ils pouvaient voir les messages d'erreur sur la sortie d'erreur standard.

Une alternative à l'écriture sur la sortie d'erreur standard est l'utilisation de fichiers journaux (logs) pour enregistrer les erreurs plutôt que de les afficher à l'écran. Cependant, cela peut être plus lourd et entraîner des problèmes de performance si le programme génère beaucoup d'erreurs.

En TypeScript, le module ```console``` fournit différentes méthodes pour écrire sur la sortie d'erreur standard, telles que ```console.error``` et ```console.warn```. L'utilisation de ces méthodes peut être utile pour signaler des erreurs ou des avertissements dans votre code.

## Voir aussi:
Pour en apprendre plus sur les méthodes de débogage en TypeScript, vous pouvez consulter la documentation officielle de TypeScript et des exemples de code sur le site de référence OpenClassrooms.