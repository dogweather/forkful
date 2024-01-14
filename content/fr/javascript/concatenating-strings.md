---
title:    "Javascript: Concaténation de chaînes de caractères"
keywords: ["Javascript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/javascript/concatenating-strings.md"
---

{{< edit_this_page >}}

# Pourquoi

Dans la programmation Javascript, il est commun d'avoir besoin de combiner plusieurs chaînes de caractères en une seule. Cela peut être utile lors de la construction de messages ou d'afficher des données en format lisible pour les utilisateurs. Dans cet article, nous allons discuter de la concaténation de chaînes de caractères et pourquoi elle est importante dans le développement web.

# Comment faire

La concaténation de chaînes de caractères peut être réalisée en utilisant l'opérateur "+" ou en utilisant la méthode ".concat ()". Voyons un exemple de chaque méthode :

```Javascript
// Utilisation de l'opérateur "+"
let nom = "Jean";
let message = "Bienvenue sur notre site, " + nom + "!";
console.log(message);
// Output : Bienvenue sur notre site, Jean!

// Utilisation de la méthode ".concat()"
let ville = "Paris";
let info = "Bienvenue à " .concat(ville, ", la ville des lumières!");
console.log(info);
// Output : Bienvenue à Paris, la ville des lumières!
```

Comme vous pouvez le voir, l'opérateur "+" et la méthode ".concat ()" ont le même résultat, ils combinent simplement les chaînes de caractères en une seule. Cependant, il y a quelques différences importantes à noter.

L'opérateur "+" peut être utilisé pour concaténer non seulement des chaînes de caractères, mais aussi des nombres. Par exemple :

```Javascript
// Combinaison de chaînes de caractères et de nombres avec l'opérateur "+"
let prix = 50;
let message = "Le prix est de " + prix + " euros.";
console.log(message);
// Output : Le prix est de 50 euros.
```

Par contre, la méthode ".concat ()" ne peut être utilisée qu'avec des chaînes de caractères. Si vous essayez de concaténer un nombre, il sera automatiquement converti en chaîne de caractères.

Une autre différence est que l'opérateur "+" peut également être utilisé pour effectuer des opérations arithmétiques, tandis que ".concat ()" ne le peut pas.

Enfin, il est important de noter que l'utilisation excessive de concaténation de chaînes de caractères peut avoir un impact sur les performances de votre application. Si vous avez besoin de concaténer plusieurs chaînes de caractères, il peut être plus efficace de les stocker dans un tableau et ensuite utiliser la méthode ".join ()" pour les joindre en une seule chaîne de caractères.

# Plongée en profondeur

En terminant, il est important de comprendre que la concaténation de chaînes de caractères peut être utilisée dans diverses situations en plus de simplement afficher du contenu à l'écran. Elle peut être utilisée pour construire des requêtes SQL, pour la manipulation de données et pour la création dynamique de noms de variables. Par exemple :

```Javascript
// Construction de requêtes SQL avec concaténation de chaînes de caractères
let lastName = "Smith";
let id = 123;
let query = "SELECT * FROM utilisateurs WHERE nom = '" + lastName + "' AND id = " + id + ";";
console.log(query);
// Output: SELECT * FROM utilisateurs WHERE nom = 'Smith' AND id = 123;

// Création dynamique de noms de variables
let chien = "Max";
let nom = "nom";
console.log(window[chien + nom]);
// Output: Max
```

Comme vous pouvez le voir, la concaténation de chaînes de caractères peut être très utile et polyvalente dans le développement web. Cependant, il est important de l'utiliser avec modération afin d'optimiser les performances de votre code.

# Voir aussi

Si vous souhaitez en savoir plus sur la concaténation de chaînes de caractères en Javascript, voici quelques liens utiles :

- [Documentation MDN sur la concaténation de chaînes de caractères en Javascript](https://developer.mozilla.org/fr/docs/Web/JavaScript/Reference/Global_Objects/String)
- [Tutoriel sur les opérations de chaînes de caractères en Javascript](https://www.alsacreations.com/tuto/lire/622-Javascript-manipulation-chaine-caracteres.html)
- [Exemples pratiques de concaténation de chaînes de caractères en Javascript](https://www.javascripttutorial.net/javascript-string-concatenation/)