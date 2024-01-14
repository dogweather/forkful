---
title:    "PHP: Ecrire un fichier texte"
keywords: ["PHP"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/php/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Pourquoi

Ecrire un fichier texte est une tâche courante dans le développement web. Que ce soit pour stocker des données ou générer des rapports, il est important de savoir comment le faire en utilisant PHP. Dans cet article, nous allons vous montrer comment écrire un fichier texte en utilisant PHP et vous plonger en profondeur dans cette tâche.

## Comment faire

Pour écrire un fichier texte en PHP, il faut suivre ces étapes simples :

1. Ouvrez un nouveau fichier PHP et donnez-lui un nom pertinent, par exemple "ecriture_fichier.php".

2. Utilisez la fonction `fopen()` pour ouvrir le fichier en mode écriture :

```PHP
$fichier = fopen("mon_fichier.txt", "w") or die("Impossible d'ouvrir le fichier !");
```

Dans cet exemple, nous avons créé un nouveau fichier texte appelé "mon_fichier.txt" et nous l'avons ouvert en mode écriture ("w"). Si le fichier n'existe pas, il sera créé automatiquement. Si le fichier existe déjà, son contenu sera écrasé.

3. Utilisez la fonction `fwrite()` pour écrire du contenu dans le fichier :

```PHP
fwrite($fichier, "Bonjour le monde !");
```

Cette fonction prend deux paramètres : le premier est l'objet fichier et le deuxième est la chaîne de caractères que vous voulez écrire dans le fichier.

4. N'oubliez pas de fermer le fichier lorsque vous avez fini :

```PHP
fclose($fichier);
```

Cela permet de libérer les ressources utilisées par le fichier.

Et voilà ! Vous venez d'écrire votre premier fichier texte en utilisant PHP. N'hésitez pas à expérimenter avec différentes fonctions pour découvrir toutes les possibilités.

## Plongée en profondeur

L'ouverture d'un fichier en mode écriture peut causer la perte de données si le fichier existe déjà et que vous ne voulez pas l'écraser. C'est pourquoi il est recommandé d'utiliser la fonction `file_exists()` avant d'ouvrir le fichier :

```PHP
if(file_exists("mon_fichier.txt")) {
  // Code pour gérer le fichier déjà existant
} else {
  $fichier = fopen("mon_fichier.txt", "w") or die("Impossible d'ouvrir le fichier !");
}
```

De plus, il est possible d'écrire dans un fichier texte ligne par ligne en utilisant la fonction `fgets()`. Cette fonction lit une ligne à la fois dans un fichier :

```PHP
$fichier = fopen("mon_fichier.txt", "w") or die("Impossible d'ouvrir le fichier !");

$txt = "Première ligne\n";
fwrite($fichier, $txt);

$txt = "Deuxième ligne\n";
fwrite($fichier, $txt);

fclose($fichier);
```

Le contenu du fichier "mon_fichier.txt" sera maintenant :

```
Première ligne
Deuxième ligne
```

## Voir aussi

Nous espérons que cet article vous a aidé à comprendre comment écrire un fichier texte en PHP. Pour en savoir plus sur les fonctions utilisées, consultez la documentation officielle de PHP :

- [Documentation de la fonction fopen()](https://www.php.net/manual/fr/function.fopen.php)
- [Documentation de la fonction fwrite()](https://www.php.net/manual/fr/function.fwrite.php)
- [Documentation de la fonction file_exists()](https://www.php.net/manual/fr/function.file-exists.php)