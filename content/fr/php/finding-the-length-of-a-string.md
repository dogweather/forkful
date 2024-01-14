---
title:                "PHP: Trouver la longueur d'une chaîne de caractères."
programming_language: "PHP"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/php/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Pourquoi

Trouver la longueur d'une chaîne de caractères est une compétence de base en programmation qui peut être utilisée dans de nombreux scénarios, tels que la vérification des mots de passe, la manipulation de données ou la validation de formulaires. C'est une compétence essentielle pour tout développeur PHP et peut être appliquée dans de nombreux projets.

## Comment faire

Pour trouver la longueur d'une chaîne de caractères, vous pouvez utiliser la fonction `strlen ()` en PHP. Elle prend une chaîne de caractères en paramètre et renvoie le nombre de caractères dans la chaîne, y compris les espaces. Voici un exemple de code :

```PHP
$chaine = "Bonjour tout le monde";
echo strlen($chaine);
```

Ceci va afficher `19`, car il y a 19 caractères dans la chaîne, y compris les espaces. Vous pouvez également utiliser cette fonction pour trouver la longueur d'une chaîne saisie par un utilisateur, en utilisant la méthode `$_GET` ou `$_POST` en fonction du type de formulaire.

## Plongée en profondeur

La fonction `strlen()` peut être utile dans de nombreux cas, mais il y a quelques points à noter :

- Elle compte tous les caractères, y compris les espaces et les caractères spéciaux.
- Elle ne fonctionne pas avec les chaînes multibytes, comme les chaînes en japonais ou en chinois.
- Elle ne prend pas en compte les balises HTML, elle compte simplement les caractères à partir du début de la chaîne.

Si vous rencontrez l'une de ces limitations, il existe d'autres fonctions en PHP qui peuvent vous aider à trouver la longueur d'une chaîne dans un contexte spécifique. Faites des recherches approfondies pour trouver la meilleure solution pour votre projet.

## Voir aussi

- La documentation officielle de la fonction `strlen()` en PHP : https://www.php.net/manual/fr/function.strlen.php
- Un article sur la façon de gérer les chaînes multibytes en PHP : https://dzone.com/articles/managing-multibyte-strings-in-php
- Un tutoriel sur la validation de formulaires en utilisant `strlen()` : https://www.cloudways.com/blog/php-form-validation/