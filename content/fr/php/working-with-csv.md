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

## Qu'est-ce que c'est et pourquoi ?

Le CSV est un format de fichier très répandu dans le monde de la programmation. Il permet de stocker des données sous forme de tableau, avec des valeurs séparées par des virgules. Les programmeurs utilisent souvent le CSV pour échanger des données entre différentes applications ou pour importer des données dans une base de données.

## Comment faire :

Voici un exemple de code en PHP pour lire un fichier CSV et afficher son contenu en utilisant une boucle for :

```PHP
// Ouverture du fichier
$handle = fopen("fichier.csv", "r");

// Boucle pour parcourir chaque ligne du fichier
for($row = 0; ($data = fgetcsv($handle, 1000, ",")) !== FALSE; $row++) {

    // Affichage des données de chaque ligne
    echo "Ligne " . $row . ": " . implode(", ", $data);

}

// Fermeture du fichier
fclose($handle);
```

### Résultat :

```
Ligne 0: Prénom, Nom, Age
Ligne 1: John, Doe, 25
Ligne 2: Jane, Smith, 30
```

## Zoom Plus :

### Contexte historique :

Le format CSV a été créé dans les années 1970 dans le but de faciliter l'échange de données entre différentes applications. Aujourd'hui, il continue à être largement utilisé dans le monde de la programmation.

### Alternatives :

Bien que le CSV reste un format populaire, il existe d'autres formats tels que JSON ou XML pour stocker des données structurées. Ces formats peuvent offrir des fonctionnalités plus avancées ou être mieux adaptés à certains types de données.

### Détails de mise en œuvre :

En PHP, la fonction `fgetcsv()` permet de lire les données d'un fichier CSV ligne par ligne. Un délimiteur de champ (par exemple une virgule) et un délimiteur de ligne (par exemple un retour à la ligne) doivent être spécifiés. Les données peuvent ensuite être manipulées et affichées à l'aide des fonctions de manipulation de tableaux disponibles en PHP.

## Voir aussi :

- [Documentation officielle de PHP sur la fonction `fgetcsv()`](https://www.php.net/manual/fr/function.fgetcsv.php)
- [Article sur l'historique du format CSV](https://www.howtogeek.com/348960/what-is-a-csv-file-and-how-do-i-open-it/) (en anglais)
- [Comparaison entre différents formats de fichiers de données](https://stackify.com/programming-big-data-file-formats/) (en anglais)