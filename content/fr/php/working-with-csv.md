---
title:                "Manipulation des fichiers CSV"
date:                  2024-01-19
html_title:           "Bash: Manipulation des fichiers CSV"
simple_title:         "Manipulation des fichiers CSV"
programming_language: "PHP"
category:             "PHP"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/php/working-with-csv.md"
---

{{< edit_this_page >}}

## What & Why?
Travailler avec des fichiers CSV (Comma-Separated Values) en PHP, c'est manipuler des données tabulaires. Les programmeurs les utilisent pour leur simplicité et leur interopérabilité avec des systèmes et applications variés.

## How to: 
Créer, lire et écrire dans les fichiers CSV est simple. Jetons un œil au code :

```PHP
// Créer et écrire dans un fichier CSV
$list = [["prénom", "nom", "email"], ["Jean", "Dupont", "jean.dupont@example.com"]];
$fp = fopen('fichier.csv', 'w');

foreach ($list as $fields) {
    fputcsv($fp, $fields);
}

fclose($fp);

// Lire un fichier CSV
if (($fp = fopen("fichier.csv", "r")) !== FALSE) {
    while (($data = fgetcsv($fp, 1000, ",")) !== FALSE) {
        print_r($data);
    }
    fclose($fp);
}
```

Exemple de sortie pour la lecture :
```
Array
(
    [0] => prénom
    [1] => nom
    [2] => email
)
Array
(
    [0] => Jean
    [1] => Dupont
    [2] => jean.dupont@example.com
)
```

## Deep Dive
Historiquement, les CSV sont utilisés depuis les premiers jours de l'informatique personnelle. D'autres formats comme JSON ou XML sont aussi populaires, mais les CSV restent un choix privilégié pour des raisons de performance et simplicité. En PHP, les fonctions `fgetcsv` et `fputcsv` gèrent le formatage et analysent les données, gérent les guillemets et les délimiteurs pour vous.

## See Also
Pour plus de détails, consultez la documentation officielle : 
- PHP manual sur les fonctions CSV: https://www.php.net/manual/en/ref.funchandle.php
- Bonnes pratiques pour l'importation de fichiers CSV : https://www.ietf.org/rfc/rfc4180.txt
