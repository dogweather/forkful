---
title:                "Lecture d'un fichier texte"
html_title:           "PHP: Lecture d'un fichier texte"
simple_title:         "Lecture d'un fichier texte"
programming_language: "PHP"
category:             "PHP"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/php/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Pourquoi

Si vous êtes un développeur PHP, vous savez peut-être déjà que la lecture de fichiers texte est une tâche courante dans la programmation. Mais peut-être que vous vous demandez pourquoi vous devriez vous embêter à lire cet article. La réponse est simple : apprendre à lire des fichiers texte en PHP peut vous aider à interagir avec des données externes telles que des bases de données ou des fichiers de configuration.

## Comment faire

Pour lire un fichier texte en PHP, vous devez suivre quelques étapes simples :

1. Tout d'abord, utilisez la fonction `fopen()` pour ouvrir le fichier et stockez le pointeur de fichier retourné dans une variable. Cette fonction prend deux paramètres : le nom du fichier et le mode d'ouverture.
2. Ensuite, utilisez la fonction `fgets()` pour récupérer une ligne de texte à la fois. Vous pouvez stocker cette ligne dans une variable pour la traiter plus tard.
3. Vous pouvez continuer à appeler la fonction `fgets()` jusqu'à ce que vous atteignez la fin du fichier. Une fois que cela se produit, la fonction renvoie `false`.
4. N'oubliez pas de fermer le fichier en utilisant la fonction `fclose()` une fois que vous avez terminé de le lire.

```PHP
$file = fopen("monfichier.txt", "r"); // ouvre le fichier en lecture seule

while(!feof($file)) { // tant que ce n'est pas la fin du fichier
  $line = fgets($file); // récupère une ligne de texte
  // traitez la ligne de texte ici
}

fclose($file); // ferme le fichier
```

Voici un exemple de fichier texte `monfichier.txt` :

```
Bonjour
Je suis un fichier texte.
Ceci est une autre ligne.
```

Lorsque vous exécutez le code ci-dessus, vous obtiendrez un résultat similaire au code suivant :

```
Bonjour
Je suis un fichier texte.
Ceci est une autre ligne. 
```

## Deep Dive

Maintenant que vous savez comment lire un fichier texte en PHP, voici quelques informations supplémentaires qui pourraient vous être utiles :

- Vous pouvez spécifier le mode d'ouverture du fichier comme `w` pour écrire dans un fichier ou `a` pour ajouter du contenu à un fichier existant.
- La fonction `fgets()` prend un deuxième paramètre facultatif qui spécifie le nombre maximum de caractères à lire à la fois.
- Vous pouvez également utiliser les fonctions `fgetc()` et `fgetss()` pour lire un seul caractère ou une ligne de texte, respectivement.
- En plus de la lecture, la fonction `fopen()` peut également être utilisée pour écrire ou créer un fichier.
- Vous pouvez utiliser la fonction `feof()` pour vérifier si vous avez atteint la fin du fichier.

## Voir aussi

Pour plus d'informations sur la lecture de fichiers en PHP, vous pouvez consulter les liens suivants :

- [La documentation officielle de PHP sur `fopen()`](https://www.php.net/manual/fr/function.fopen.php)
- [Un tutoriel sur la lecture et l'écriture de fichiers en PHP](https://www.tutorialspoint.com/php/php_files.htm)
- [Des exemples pratiques de lecture et d'écriture de fichiers en PHP](https://www.w3schools.com/php/php_file_create.asp)