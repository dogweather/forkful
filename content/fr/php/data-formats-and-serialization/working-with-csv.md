---
title:                "Travailler avec CSV"
date:                  2024-02-03T19:20:43.143880-07:00
model:                 gpt-4-0125-preview
simple_title:         "Travailler avec CSV"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/php/working-with-csv.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?

Travailler avec des fichiers CSV (Comma-Separated Values ou Valeurs Séparées par des Virgules) implique de lire et d’écrire des données dans des fichiers CSV, un format populaire pour représenter des données tabulaires en texte brut. Les programmeurs le font pour échanger facilement des données entre différents programmes, systèmes, ou bases de données, grâce à sa simplicité et à son large support à travers les plateformes et langages de programmation.

## Comment faire :

PHP fournit des fonctions intégrées pour la manipulation de fichiers CSV, rendant la lecture et l'écriture dans ces fichiers directes sans avoir besoin de bibliothèques tierces. Voici des exemples pour vous aider à démarrer :

### Lire un fichier CSV

Vous pouvez ouvrir un fichier CSV et lire son contenu en utilisant `fopen()` en combinaison avec `fgetcsv()` :

```php
<?php
$filename = 'data.csv';
$handle = fopen($filename, "r");
if ($handle !== FALSE) {
    while (($data = fgetcsv($handle, 1000, ",")) !== FALSE) {
        $num = count($data);
        echo "Nombre de champs dans la ligne : $num\n";
        for ($c = 0; $c < $num; $c++) {
            echo $data[$c] . "\n";
        }
    }
    fclose($handle);
}
?>
```

Ce script imprime le nombre de champs de chaque ligne suivi par le contenu de chaque champ.

### Écrire dans un fichier CSV

Pour écrire dans un fichier CSV, utilisez `fopen()` en mode écriture (`w`) et `fputcsv()` :

```php
<?php
$list = [
    ['ID', 'Nom', 'Email'],
    [1, 'John Doe', 'john@example.com'],
    [2, 'Jane Doe', 'jane@example.com']
];

$handle = fopen('users.csv', 'w');

foreach ($list as $row) {
    fputcsv($handle, $row);
}

fclose($handle);
?>
```

Ce script crée un fichier nommé `users.csv` et y écrit l'en-tête et deux lignes de données.

### Utiliser une bibliothèque : League\Csv

Pour une manipulation CSV plus avancée, la bibliothèque `League\Csv` offre un ensemble robuste de fonctionnalités. Après l'avoir installée via Composer (`composer require league/csv`), vous pouvez l'utiliser pour lire et écrire des données CSV de manière plus flexible.

#### Lire avec League\Csv

```php
<?php
require 'vendor/autoload.php';

use League\Csv\Reader;

$csv = Reader::createFromPath('data.csv', 'r');
$csv->setHeaderOffset(0); // Définissez-le si vous souhaitez utiliser la première ligne comme en-tête

$results = $csv->getRecords();
foreach ($results as $row) {
    print_r($row);
}
?>
```

Ce script lit `data.csv`, traitant la première ligne comme des en-têtes de colonne et imprime chaque ligne en tant que tableau associatif.

#### Écrire avec League\Csv

```php
<?php
require 'vendor/autoload.php';

use League\Csv\Writer;

$csv = Writer::createFromPath('users_new.csv', 'w+');

$csv->insertOne(['ID', 'Nom', 'Email']);
$csv->insertAll([
    [3, 'Alex Doe', 'alex@example.com'],
    [4, 'Anna Smith', 'anna@example.com']
]);

echo "Écrit dans users_new.csv avec succès.";
?>
```

Cela crée `users_new.csv` et écrit une ligne d'en-tête suivie de deux lignes de données.
