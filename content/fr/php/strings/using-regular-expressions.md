---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:17:31.244579-07:00
description: "Les expressions r\xE9guli\xE8res (regex) en PHP sont des motifs utilis\xE9\
  s pour rechercher des combinaisons de caract\xE8res dans des cha\xEEnes de texte,\
  \ permettant\u2026"
lastmod: '2024-03-13T22:44:57.867539-06:00'
model: gpt-4-0125-preview
summary: "Les expressions r\xE9guli\xE8res (regex) en PHP sont des motifs utilis\xE9\
  s pour rechercher des combinaisons de caract\xE8res dans des cha\xEEnes de texte,\
  \ permettant\u2026"
title: "Utilisation des expressions r\xE9guli\xE8res"
weight: 11
---

## Quoi & Pourquoi ?

Les expressions régulières (regex) en PHP sont des motifs utilisés pour rechercher des combinaisons de caractères dans des chaînes de texte, permettant des opérations de recherche et de remplacement sophistiquées ainsi que la validation de données. Les programmeurs utilisent les regex pour leur puissance et leur flexibilité lors de l'analyse de texte, la validation de formulaires ou le scraping de données web, ce qui en fait un outil indispensable dans l'arsenal d'un développeur.

## Comment utiliser :

PHP prend en charge les expressions régulières via la bibliothèque PCRE (Perl Compatible Regular Expressions), offrant un riche ensemble de fonctions. Voici comment les utiliser :

### Correspondance à un motif :

Pour vérifier si un motif existe dans une chaîne, utilisez `preg_match()`. Cette fonction retourne 1 si le motif a été trouvé dans la chaîne et 0 sinon.

```php
if (preg_match("/\bweb\b/i", "PHP est un langage de script web")) {
    echo "Une correspondance a été trouvée.";
} else {
    echo "Aucune correspondance n'a été trouvée.";
}
// Sortie : Une correspondance a été trouvée.
```

### Trouver toutes les correspondances :

`preg_match_all()` est utilisé lorsque vous avez besoin de trouver toutes les occurrences d'un motif dans une chaîne.

```php
$text = "chats et chiens";
$pattern = "/\b([a-z]+)\b/i";
preg_match_all($pattern, $text, $matches);
print_r($matches[0]);
// Sortie : Array ( [0] => chats [1] => et [2] => chiens )
```

### Remplacer du texte :

Pour remplacer du texte qui correspond à une expression régulière, `preg_replace()` est utilisé. Il est incroyablement puissant pour formater et nettoyer des données.

```php
$originalText = "15 avril 2003";
$pattern = "/(\w+) (\d+), (\d+)/i";
$replacement = '${1}1,$3';
echo preg_replace($pattern, $replacement, $originalText);
// Sortie : avril1,2003
```

### Fractionner des chaînes :

Vous pouvez diviser une chaîne en un tableau en utilisant `preg_split()`, en spécifiant un motif pour le délimiteur.

```php
$text = "PHP est, un langage de script, extrêmement populaire";
$parts = preg_split("/,\s*/", $text);
print_r($parts);
// Sortie : Array ( [0] => PHP est [1] => un langage de script [2] => extrêmement populaire )
```

De plus, pour des motifs et des tâches regex complexes, des cadres et des bibliothèques tels que le composant `Finder` de Symfony ou la collection de fonctions d'aide de Laravel peuvent fournir une couche d'abstraction plus pratique. Cependant, comprendre et utiliser les fonctions PCRE intégrées de PHP est crucial pour un traitement et une validation de texte efficaces directement dans les scripts PHP.
