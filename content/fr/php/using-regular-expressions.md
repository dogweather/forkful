---
title:                "Utiliser les expressions régulières."
html_title:           "PHP: Utiliser les expressions régulières."
simple_title:         "Utiliser les expressions régulières."
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/php/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Qu'est-ce que c'est et pourquoi les programmeurs l'utilisent-ils?
Les expressions régulières sont une technique de traitement de chaînes de caractères qui permettent aux programmeurs de rechercher et de manipuler des motifs spécifiques dans des données textuelles. Ils les utilisent pour simplifier les tâches de filtrage, de vérification et de modification de données, en leur permettant de trouver et de remplacer des parties de texte en utilisant des motifs plutôt que des correspondances exactes.

## Comment faire:
```PHP
//Exemple 1: Rechercher et remplacer un motif dans une chaîne de caractères
$texte = "Bienvenue sur notre site web, vous pouvez trouver des produits à prix abordable!";
$nouveau_texte = preg_replace("/abordable/", "bon marché", $texte);
echo $nouveau_texte;
```

Output: Bienvenue sur notre site web, vous pouvez trouver des produits à prix bon marché!

```PHP
//Exemple 2: Vérifier si une chaîne de caractères correspond à un motif spécifique
$texte = "Le mot de passe doit contenir au moins une majuscule, une minuscule et un chiffre.";
if (preg_match("/^[A-Z]+[a-z]+[0-9]+$/", $texte)) {
  echo "Le mot de passe satisfait aux exigences de sécurité.";
} else {
  echo "Le mot de passe ne satisfait pas aux exigences de sécurité.";
}
```

Output: Le mot de passe satisfait aux exigences de sécurité.

## Exploration approfondie:
Les expressions régulières ont été inventées dans les années 1950 par le mathématicien Stephen Kleene, mais ont été rendues populaires par l'utilisation dans les langages informatiques telles que le Perl et plus tard PHP. Bien qu'elles soient très utiles pour la manipulation de données textuelles, il existe également d'autres méthodes telles que les fonctions de manipulation de chaînes de caractères intégrées dans PHP telles que "substr" et "strpos". Cependant, les expressions régulières offrent plus de flexibilité et de puissance dans la recherche et la manipulation de motifs complexes.

## Voir aussi:
- [Documentation PHP pour les expressions régulières](https://www.php.net/manual/fr/function.preg-match.php)
- [Tutoriel pour les débutants sur les expressions régulières en PHP](https://www.w3schools.com/php/php_regex.asp)
- [Fonctions de manipulation de chaînes de caractères intégrées dans PHP](https://www.php.net/manual/fr/ref.strings.php)