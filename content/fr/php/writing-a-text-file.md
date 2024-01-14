---
title:    "PHP: Écrire un fichier texte"
keywords: ["PHP"]
---

{{< edit_this_page >}}

## Pourquoi

Vous vous demandez peut-être pourquoi quelqu'un écrirait un fichier texte en utilisant PHP. Eh bien, cela peut être utile pour stocker des données dans un format lisible par l'homme et facilement manipulable avec du code.

## Comment faire

Pour écrire un fichier texte en utilisant PHP, vous devez suivre ces étapes simples:

```PHP
// Ouverture du fichier en mode écriture
$fichier = fopen('mon_fichier.txt', 'w');

// Écriture d'une chaîne dans le fichier
$texte = "Bonjour le monde !";
fwrite($fichier, $texte);

// Fermeture du fichier
fclose($fichier);
```

Vous pouvez également écrire plusieurs lignes dans votre fichier en utilisant la fonction `fwrite()` plusieurs fois.

## Plongée en profondeur

Maintenant que vous savez comment écrire un fichier texte en utilisant PHP, explorons quelques fonctionnalités avancées.

Tout d'abord, vous pouvez ajouter des données à la fin d'un fichier existant en utilisant le mode d'ouverture `a` au lieu de `w` dans la fonction `fopen()`. De plus, vous pouvez utiliser la fonction `fputs()` à la place de `fwrite()` si vous préférez.

Si vous souhaitez écrire des données de manière binaire, vous pouvez utiliser le mode `wb` à la place de `w` dans la fonction `fopen()`. Cela est utile si vous souhaitez stocker des images, des fichiers audio ou d'autres types de fichiers dans un fichier texte.

N'oubliez pas de toujours fermer votre fichier après avoir fini d'écrire afin d'éviter toute perte de données ou de problèmes de performances.

## Voir aussi

- [Documentation officielle sur la fonction `fopen()` de PHP](https://www.php.net/manual/fr/function.fopen.php)
- [Guide sur la manipulation de fichiers texte en PHP](https://www.php.net/manual/fr/book.filesystem.php)
- [Tutoriel sur la gestion des fichiers en PHP par OpenClassrooms](https://openclassrooms.com/fr/courses/1665806-programmez-en-oriente-objet-en-php/1669822-la-gestion-des-fichiers)

Merci d'avoir lu cet article sur l'écriture de fichiers texte en PHP ! Nous espérons que cela vous sera utile dans vos futurs projets de programmation. N'hésitez pas à explorer davantage les fonctionnalités telles que la lecture de fichiers texte, la manipulation de leurs contenus et la gestion des erreurs. À bientôt !