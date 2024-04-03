---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:05:52.245010-07:00
description: "Mettre en majuscule une cha\xEEne implique de modifier le premier caract\xE8\
  re d'un texte donn\xE9 en majuscule, afin que les phrases, les titres ou les noms\u2026"
lastmod: '2024-03-13T22:44:57.860291-06:00'
model: gpt-4-0125-preview
summary: "Mettre en majuscule une cha\xEEne implique de modifier le premier caract\xE8\
  re d'un texte donn\xE9 en majuscule, afin que les phrases, les titres ou les noms\
  \ propres commencent correctement dans un ensemble de donn\xE9es."
title: "Mettre en majuscule une cha\xEEne"
weight: 2
---

## Comment faire :
PHP prend en charge nativement diverses fonctions pour mettre en majuscule les chaînes, chacune servant un objectif différent. Voici comment vous pouvez les utiliser :

### Mettre en majuscule la première lettre d'une chaîne :
```php
$string = "bonjour, monde !";
$capitalizedString = ucfirst($string);
echo $capitalizedString; // Affiche : Bonjour, monde !
```

### Mettre en majuscule la première lettre de chaque mot :
```php
$string = "bonjour, monde !";
$capitalizedWords = ucwords($string);
echo $capitalizedWords; // Affiche : Bonjour, Monde !
```

### Convertir toute la chaîne en majuscules :
```php
$string = "bonjour, monde !";
$upperCaseString = strtoupper($string);
echo $upperCaseString; // Affiche : BONJOUR, MONDE !
```

Pour les scénarios nécessitant plus de personnalisation ou des solutions tierces, des bibliothèques comme `mbstring` (pour les chaînes multioctets) peuvent être utilisées, en particulier lorsqu'il s'agit d'internationalisation où les caractères peuvent aller au-delà de l'ensemble de caractères ASCII de base.

### Utiliser mbstring pour mettre en majuscule les chaînes UTF-8 :
Assurez-vous que l'extension `mbstring` est activée dans votre configuration PHP, puis :

```php
$string = "élégant";
$capitalizedString = mb_convert_case($string, MB_CASE_TITLE, "UTF-8");
echo $capitalizedString; // Affiche : Élégant
```

Cette approche aide à mettre en majuscule avec précision les chaînes qui incluent des caractères non-ASCII, en respectant les nuances de diverses langues.
