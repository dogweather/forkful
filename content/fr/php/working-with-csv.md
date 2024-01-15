---
title:                "Travailler avec des fichiers csv"
html_title:           "PHP: Travailler avec des fichiers csv"
simple_title:         "Travailler avec des fichiers csv"
programming_language: "PHP"
category:             "PHP"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/php/working-with-csv.md"
---

{{< edit_this_page >}}

# Pourquoi travailler avec des fichiers CSV en PHP ?

Travailler avec des fichiers CSV en PHP peut être très utile lors de la manipulation de données tabulaires. Les fichiers CSV sont faciles à lire et à écrire, leur format est léger et ils peuvent être ouverts et traités par de nombreux programmes différents.

## Comment faire ?

L'utilisation de fichiers CSV en PHP est assez simple. Tout d'abord, il est important de spécifier que le fichier CSV est un fichier texte avec une extension .csv. Ensuite, voici les étapes à suivre pour lire et écrire des données CSV en PHP :

1. Ouvrez le fichier CSV en utilisant la fonction fopen() et spécifiez le mode d'ouverture en tant que "lecture" ou "écriture".
```
$csv_file = fopen('fichier.csv', 'r');
```

2. Lisez le contenu du fichier à l'aide de la fonction fgetcsv(), qui lit chaque ligne du fichier CSV et la stocke dans un tableau.
```
while (($csv_data = fgetcsv($csv_file)) !== FALSE) {
  // Traitement des données lues
}
```

3. Pour écrire des données dans un fichier CSV, utilisez la fonction fputcsv() en spécifiant le tableau contenant les données à écrire et le séparateur de données (par défaut, c'est une virgule).
```
$data = array('John', 'Doe', 'johndoe@email.com');
fputcsv($csv_file, $data);
```

4. N'oubliez pas de fermer le fichier après avoir terminé toutes les opérations en utilisant la fonction fclose().
```
fclose($csv_file);
```

Voici un exemple complet de lecture et d'écriture de données CSV en PHP :
```
$csv_file = fopen('fichier.csv', 'r');

while (($csv_data = fgetcsv($csv_file)) !== FALSE) {
  // Lisez et traitez les données
}

$data = array('John', 'Doe', 'johndoe@email.com');
fputcsv($csv_file, $data);

fclose($csv_file);
```

## Plongée en profondeur

Il existe également des fonctions spécifiques pour manipuler des données CSV dans PHP, telles que fgetcsv() pour lire une ligne du fichier, fputcsv() pour écrire une ligne et fgetcvs() pour compter le nombre de lignes dans le fichier.

Il est également possible de spécifier un délimiteur de données différent de la virgule en utilisant la fonction fgetcsv(). De plus, PHP dispose de fonctions pour vérifier si un fichier CSV existe, le supprimer ou le renommer.

Enfin, il est important de noter que les données CSV peuvent être sauvegardées et lues sous forme de tableaux en utilisant les fonctions csv_encode() et csv_decode().

# Voir aussi
- Documentation officielle de PHP sur les fonctions CSV : https://www.php.net/manual/fr/ref.csv.php
- Tutoriel sur la manipulation de fichiers CSV en PHP : https://www.tutorialspoint.com/php/php_and_csv.htm