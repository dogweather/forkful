---
title:    "Javascript: Mise en majuscule d'une chaîne de caractères"
keywords: ["Javascript"]
---

{{< edit_this_page >}}

# Pourquoi

La capitalisation d'une chaîne de caractères est un aspect important de la programmation Javascript car elle permet de rendre le texte plus lisible et compréhensible pour les utilisateurs. En mettant uniquement la première lettre en majuscule, cela donne une certaine forme de structure à votre texte et peut également faciliter la recherche de mots clés.

# Comment faire

Pour capitaliser une chaîne de caractères en Javascript, il existe différentes méthodes, en voici quelques-unes:

```Javascript
// Méthode 1: Utiliser la fonction toUpperCase () pour mettre en majuscule la première lettre
let chaine = "bonjour tout le monde";

chaine = chaine[0].toUpperCase() + chaine.slice(1); // chaine vaut maintenant "Bonjour tout le monde"

// Méthode 2: Utiliser la fonction replace () pour remplacer la première lettre en minuscule par sa version en majuscule
chaine = chaine.replace(chaine.charAt(0), chaine.charAt(0).toUpperCase()); // chaine vaut également "Bonjour tout le monde"

// Méthode 3: Utiliser la fonction charAt () pour accéder à la première lettre et la transformer en majuscule
chaine = chaine.charAt(0).toUpperCase() + chaine.substring(1); // encore une fois, chaine est égale à "Bonjour tout le monde"
```

# Plongée en profondeur

Bien que ces méthodes soient simples et efficaces pour capitaliser une chaîne de caractères, il est important de comprendre leur fonctionnement en profondeur. En Javascript, les chaînes de caractères sont des objets et donc, vous pouvez utiliser des méthodes sur ces derniers. Les méthodes utilisées dans les exemples ci-dessus font appel à des propriétés et des fonctions telles que toUpperCase(), replace(), charAt() et substring() pour modifier la chaîne initiale et lui donner une nouvelle forme.

Il est également important de noter qu'il existe des bibliothèques externes telles que Lodash qui offrent des fonctions prédéfinies pour la manipulation de chaînes de caractères, y compris la capitalisation.

# Voir aussi

- [Documentation officielle de Javascript sur les chaînes de caractères](https://developer.mozilla.org/fr/docs/Web/JavaScript/Reference/Objets_globaux/String)
- [Documentation officielle de Lodash sur la manipulation des chaînes de caractères](https://lodash.com/docs/4.17.15#capitalize)