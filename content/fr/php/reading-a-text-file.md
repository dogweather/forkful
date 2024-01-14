---
title:                "PHP: Lecture d'un fichier texte"
simple_title:         "Lecture d'un fichier texte"
programming_language: "PHP"
category:             "PHP"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/php/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Pourquoi lire un fichier texte en PHP?

Lire un fichier texte en PHP est une tâche courante dans la programmation web. Cela peut être utile pour récupérer des données à partir de fichiers texte ou pour créer des fichiers texte avec du contenu généré par le code. Dans cet article, nous couvrirons les bases de la lecture d'un fichier texte en utilisant PHP.

## Comment faire

Pour lire un fichier texte en PHP, nous utiliserons la fonction native "fopen()" pour ouvrir le fichier, puis la fonction "fgets()" pour lire chaque ligne dans une boucle jusqu'à ce que nous atteignons la fin du fichier. Voici un exemple de code:

```PHP
$file = fopen("fichier.txt", "r"); // ouvrir le fichier en mode lecture
if ($file) { // vérifier si le fichier a été ouvert avec succès
   while (($line = fgets($file)) !== false) { // boucler jusqu'à ce que nous atteignons la fin du fichier
       echo $line; // afficher chaque ligne
   }
   if (!feof($file)) { // vérifier si la fin du fichier a été atteint
       echo "Erreur: fin de fichier non atteinte.";
   }
   fclose($file); // fermer le fichier
}
```

Si nous avons le fichier suivant "fichier.txt":

```
Bonjour,
Comment ça va?
```

Le code ci-dessus affichera:

```
Bonjour,
Comment ça va?
```

## Plongée en profondeur

Il est important de noter que la fonction "fgets()" lit le fichier ligne par ligne, donc chaque ligne sera stockée dans la variable "$line". Si nous avons besoin de lire le fichier dans son intégralité, nous pouvons utiliser la fonction "file_get_contents()". De plus, nous pouvons également spécifier l'option "flags" dans la fonction "fopen()" pour définir le mode de lecture, écriture ou création du fichier. Pour plus d'informations sur ces fonctions, vous pouvez consulter la documentation officielle de PHP.

## Voir aussi

- Documentation officielle PHP pour "fopen()": https://www.php.net/manual/fr/function.fopen.php
- Documentation officielle PHP pour "fgets()": https://www.php.net/manual/fr/function.fgets.php
- Documentation officielle PHP pour "file_get_contents()": https://www.php.net/manual/fr/function.file-get-contents.php