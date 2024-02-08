---
title:                "Utilisation des tableaux associatifs"
aliases:
- fr/php/using-associative-arrays.md
date:                  2024-01-30T19:12:09.350947-07:00
model:                 gpt-4-0125-preview
simple_title:         "Utilisation des tableaux associatifs"

tag:                  "Data Structures"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/php/using-associative-arrays.md"
changelog:
  - 2024-01-30, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?

Les tableaux associatifs en PHP sont comme des listes surpuissantes où chaque élément peut être accédé en utilisant une clé lisible par l'humain au lieu de simples numéros. Les programmeurs les utilisent pour stocker et manipuler des données de manière plus intuitive, permettant ainsi un code plus facile à lire et plus maintenable.

## Comment faire :

En PHP, créer et utiliser des tableaux associatifs est simple. Voici un rapide aperçu :

```PHP
<?php
// Création d'un tableau associatif
$person = array(
    "name" => "John Doe",
    "age" => 30,
    "email" => "john@example.com"
);

// Alternativement, la syntaxe de tableau courte
$person = [
    "name" => "John Doe",
    "age" => 30,
    "email" => "john@example.com"
];

// Accéder aux valeurs en utilisant les clés
echo "Nom : " . $person["name"] . "\n";
echo "Âge : " . $person["age"] . "\n";
echo "Email : " . $person["email"] . "\n";

// Modifier une valeur
$person["age"] = 31;

// Ajouter une nouvelle paire clé-valeur
$person["pays"] = "USA";

// Itérer sur un tableau associatif
foreach ($person as $key => $value) {
    echo $key . " : " . $value . "\n";
}

// Sortie
// Nom : John Doe
// Âge : 31
// Email : john@example.com
// pays : USA
?>
```

Remarquez comment les clés peuvent être n'importe quelle chaîne, vous permettant d'accéder aux éléments en utilisant ces clés plutôt que des indices numériques, qui peuvent être moins significatifs et plus difficiles à se rappeler.

## Plongée profonde

Les tableaux associatifs en PHP sont implémentés en interne à l'aide de tables de hachage qui fournissent un accès très rapide aux éléments par clé, les rendant hautement efficaces pour de nombreuses tâches. Cette efficacité, combinée à leur facilité d'utilisation, fait des tableaux associatifs une pierre angulaire de la programmation PHP.

Historiquement, les tableaux de PHP (à la fois indexés et associatifs) ont été incroyablement flexibles, leur permettant de servir de listes, piles, files d'attente, et plus encore. Cependant, cette flexibilité peut parfois conduire à un code moins efficace si elle n'est pas utilisée avec prudence.

Récemment, avec les améliorations de la programmation orientée objet en PHP, certains développeurs préfèrent utiliser des objets pour les données structurées, particulièrement pour les ensembles de données complexes ou interdépendants. Utiliser des classes peut offrir une meilleure encapsulation et abstraction, rendre le code plus facile à tester et clarifier les intentions. Cependant, pour le stockage simple de clé-valeur et les scénarios de manipulation de données simples, les tableaux associatifs restent un excellent choix en raison de leur simplicité et de la syntaxe intuitive.
