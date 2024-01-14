---
title:                "PHP: Travailler avec les fichiers csv"
simple_title:         "Travailler avec les fichiers csv"
programming_language: "PHP"
category:             "PHP"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/php/working-with-csv.md"
---

{{< edit_this_page >}}

## Pourquoi

Les fichiers CSV (Comma Separated Values) sont un moyen courant de stocker des données dans un format facile à lire et à manipuler. En utilisant PHP pour travailler avec des fichiers CSV, vous pouvez facilement lire et écrire des données tabulaires dans vos programmes, ce qui peut être utile pour de nombreuses tâches, telles que l'importation ou l'exportation de données à partir de bases de données.

## Comment faire

Pour travailler avec des fichiers CSV en PHP, vous pouvez utiliser la fonction `fgetcsv()` pour lire les données d'un fichier CSV dans un tableau. Voici un exemple de code pour lire et afficher les données d'un fichier CSV :

```PHP
fichier = fopen("donnees.csv", "r");
while (($donnees = fgetcsv($fichier, 1000, ",")) !== FALSE) {
    print_r($donnees);
}
fclose($fichier);
```

Voici un exemple du résultat de ce code pour un fichier CSV contenant deux colonnes :

```PHP
Array
(
    [0] => Nom
    [1] => Age
)
Array
(
    [0] => Marie
    [1] => 25
)
Array
(
    [0] => Pierre
    [1] => 30
)
Array
(
    [0] => Lucie
    [1] => 27
)
```

Vous pouvez également utiliser la fonction `fputcsv()` pour écrire des données dans un fichier CSV. Voici un exemple de code pour créer un nouveau fichier CSV et y écrire des données :

```PHP
fichier = fopen("nouveau.csv", "w");
$donnees = array(
    array("ID", "Nom", "Age"),
    array(1, "Marie", 25),
    array(2, "Pierre", 30),
    array(3, "Lucie", 27)
);
foreach ($donnees as $ligne) {
    fputcsv($fichier, $ligne);
}
fclose($fichier);
```

Et voici un exemple du résultat de ce code dans le fichier CSV :

| ID | Nom   | Age |
|----|-------|-----|
| 1  | Marie | 25  |
| 2  | Pierre| 30  |
| 3  | Lucie | 27  |

## Plongée en profondeur

Pour plus d'informations sur la manipulation de fichiers CSV en PHP, vous pouvez consulter la documentation officielle de PHP sur les fonctions `fgetcsv()` et `fputcsv()`. Vous pouvez également explorer d'autres fonctions utiles telles que `array_map()` et `array_combine()` pour traiter et organiser les données du tableau.

Il est également important de garder à l'esprit que la sécurité est importante lors du travail avec des données CSV. Assurez-vous de valider et de nettoyer les données avant de les utiliser pour éviter les vulnérabilités potentielles.

## Voir aussi

- [Documentation PHP sur fgetcsv()](https://www.php.net/manual/fr/function.fgetcsv.php)
- [Documentation PHP sur fputcsv()](https://www.php.net/manual/fr/function.fputcsv.php)
- [Documentation PHP sur array_map()](https://www.php.net/manual/fr/function.array-map.php)
- [Documentation PHP sur array_combine()](https://www.php.net/manual/fr/function.array-combine.php)