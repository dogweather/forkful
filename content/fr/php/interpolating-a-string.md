---
title:                "Interpolation d'une chaîne de caractères"
html_title:           "Ruby: Interpolation d'une chaîne de caractères"
simple_title:         "Interpolation d'une chaîne de caractères"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/php/interpolating-a-string.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi?
L'interpolation de chaînes en PHP permet d'insérer des variables directement dans une chaîne de caractères. Cela permet une écriture plus simple et plus lisible du code.

## Comment faire:
Voyons un exemple simple d'interpolation de chaîne en PHP:

```PHP
$nom = 'Jean';
echo "Bonjour, $nom!";
```
Dans ce cas, le message 'Bonjour, Jean!' sera affiché.

Vous pouvez également utiliser des fonctions dans votre chaîne interpolée:

```PHP
$heure = date('H');
echo "Il est actuellement ${heure}h.";
```
Ici, la fonction `date('H')` est utilisée pour afficher l'heure actuelle.

## Plongée en profondeur
L'interpolation de chaînes a été incorporée à PHP depuis sa première version. C'est une caractéristique empruntée à Perl, qui simplifie l'écriture du code.

Des alternatives à l'interpolation de chaînes incluent l'utilisation de la fonction `printf()` ou la concaténation avec le symbole `.`. Cependant, ces techniques peuvent rendre le code plus complexe à lire.

La mise en œuvre interne de l'interpolation des chaînes est assez simple en PHP. Le compilateur remplace simplement les expressions entre `{}` ou les variables précédées d'un `$` par leurs valeurs respectives, avant l'exécution du code.

## Voir aussi

- [Documentation PHP sur l'interpolation de chaînes](https://www.php.net/manual/fr/language.types.string.php#language.types.string.parsing)
- [Article W3Schools sur l'Interpolation de chaînes en PHP](https://www.w3schools.com/php/php_string.asp)