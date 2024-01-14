---
title:    "PHP: Fusion de chaînes de caractères"
keywords: ["PHP"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/php/concatenating-strings.md"
---

{{< edit_this_page >}}

## Pourquoi

La concaténation de chaînes de caractères est une pratique courante en programmation PHP. Elle permet de combiner plusieurs chaînes de caractères pour en former une seule, ce qui peut être très utile dans de nombreux cas. Dans cet article, nous allons explorer comment concaténer des chaînes en PHP et pourquoi c'est une compétence précieuse pour les développeurs.

## Comment faire

La concaténation de chaînes en PHP est très simple. Il suffit d'utiliser l'opérateur `.` pour joindre les différentes chaînes. Voici un exemple de code qui concatène deux chaînes et affiche le résultat :

```PHP
$phrase1 = "Bonjour";
$phrase2 = "le monde";
$resultat = $phrase1 . " " . $phrase2;
echo $resultat;
```

**Résultat :** Bonjour le monde

Comme vous pouvez le voir, les deux phrases ont été combinées pour former une seule phrase complète. Vous pouvez également concaténer des variables avec des chaînes de caractères, comme dans cet exemple :

```PHP
$nom = "Marie";
$salutation = "Bonjour $nom, comment ça va ?";
echo $salutation;
```

**Résultat :** Bonjour Marie, comment ça va ?

Il est également possible de concaténer plus de deux chaînes en utilisant plusieurs opérateurs `.`. Par exemple :

```PHP
$prenom = "Pierre";
$nom = "Dupont";
$ville = "Paris";
$phrase = "Bonjour, je m'appelle " . $prenom . " " . $nom . " et j'habite à " . $ville . ".";
echo $phrase;
```

**Résultat :** Bonjour, je m'appelle Pierre Dupont et j'habite à Paris.

## Plongée en profondeur

La concaténation de chaînes est une compétence de base en PHP, mais elle peut également être utilisée de manière plus avancée. Par exemple, vous pouvez concaténer plusieurs chaînes dans une boucle pour créer une chaîne plus complexe. Elle peut également être utilisée avec des fonctions telles que `sprintf()` pour formater dynamiquement les chaînes de caractères.

Il est important de noter que la concaténation de chaînes peut être coûteuse en termes de performances si elle est utilisée de manière excessive. Il est donc recommandé d'utiliser la concaténation avec parcimonie et d'éviter de l'utiliser dans des boucles qui peuvent être exécutées de nombreuses fois.

## Voir aussi

- [Documentation sur la concaténation en PHP](https://www.php.net/manual/fr/language.operators.string.php)
- [Exemples de concaténation en PHP](https://www.w3schools.com/php/php_operators.asp)
- [Bonnes pratiques pour l'utilisation de la concaténation en PHP](https://www.geeksforgeeks.org/php-how-to-optimize-concatenation-of-strings/)