---
title:                "PHP: Extraction de sous-chaînes"
programming_language: "PHP"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/php/extracting-substrings.md"
---

{{< edit_this_page >}}

# Pourquoi

Vous vous êtes probablement déjà retrouvé dans une situation où vous aviez besoin de prendre une partie spécifique d'une chaîne de caractères. Peut-être que vous aviez besoin du nom de famille dans un formulaire, ou du numéro de téléphone dans une liste de contacts. Dans ces cas-là, l'extraction de sous-chaînes peut être très utile.

# Comment faire

Heureusement, PHP dispose d'une fonction très pratique pour extraire des sous-chaînes : `substr()`. Cette fonction prend trois paramètres : la chaîne de caractères originale, l'index de départ et la longueur de la sous-chaîne souhaitée.

Voici un exemple concret :

```PHP
$nom = "Dupont";

// On utilise la fonction substr() pour extraire les deux premiers caractères
$prenom = substr($nom, 0, 2); // $prenom vaut "Du"

// On peut également extraire à partir de la fin de la chaîne en utilisant un indice négatif
$numero = substr($nom, -3); // $numero vaut "ont"
```

Il est également possible d'utiliser la fonction `mb_substr()` pour gérer correctement les caractères multi-octets, comme les caractères spéciaux français.

# Plongée en profondeur

La fonction `substr()` peut être utilisée de différentes manières en combinant les paramètres d'index et de longueur. Voici quelques cas d'utilisation courants :

- Pour extraire une partie d'une URL, par exemple le nom de domaine :
```PHP
$url = "https://example.com/nom-de-page.html";
$domaine = substr($url, 8, 17); // $domaine vaut "example.com"
```

- Pour récupérer une partie d'une chaîne à partir d'un point spécifique :
```PHP
$chaine = "Bonjour tout le monde !";
$message = substr($chaine, 8); // $message vaut "tout le monde !"
```

- Pour extraire une sous-chaîne d'une longueur prédéfinie à partir d'une position aléatoire :
```PHP
$chaine = "Lorem ipsum dolor sit amet";
$morceau = substr($chaine, rand(0, 15), 5); // $morceau vaut un morceau aléatoire de 5 caractères à partir de la 16ème position
```

# Voir aussi

- [Documentation officielle de substr()](https://www.php.net/manual/fr/function.substr.php)
- [Documentation officielle de mb_substr()](https://www.php.net/manual/fr/function.mb-substr.php)
- [Fonctions de manipulation de chaînes en PHP](https://www.php.net/manual/fr/ref.strings.php)