---
title:    "Rust: Concaténation de chaînes de caractères"
keywords: ["Rust"]
---

{{< edit_this_page >}}

## Pourquoi

La concaténation de chaînes de caractères est une méthode couramment utilisée en programmation pour combiner plusieurs morceaux de texte en un seul. Cette technique est particulièrement utile en Rust, car elle permet de créer des chaînes de caractères dynamiques et d'interagir avec des données externes telles que des fichiers ou des utilisateurs.

## Comment faire

Pour concaténer des chaînes de caractères en Rust, vous pouvez utiliser l'opérateur `+` ou la méthode `format!()`. Par exemple, pour concaténer les chaînes "Bonjour" et "monde", vous pouvez utiliser :

```Rust
let str1 = "Bonjour";
let str2 = "monde";
let concatenated = str1 + str2;
```

Cela donnera une sortie de "Bonjourmonde". Vous pouvez également utiliser la méthode `format!()` pour ajouter des variables ou des valeurs à vos chaînes concaténées, comme ceci :

```Rust
let name = "John";
let age = 25;
let greeting = format!("Bonjour, je m'appelle {} et j'ai {} ans.", name, age);
```

Cela donnera une sortie de "Bonjour, je m'appelle John et j'ai 25 ans."

## Plongée en profondeur

Il est important de noter que la concaténation de chaînes de caractères en Rust peut être coûteuse en termes de performances si elle est utilisée fréquemment dans une boucle. Cela est dû au fait que la concaténation crée un nouvel objet chaque fois qu'elle est utilisée, ce qui peut entraîner des réallocations de mémoire. Pour éviter cela, il est conseillé d'utiliser la macro `format!()` et d'utiliser des références plutôt que des valeurs.

De plus, il est important de faire attention à l'emplacement de la concaténation dans votre code. En général, il est préférable de concaténer des chaînes de caractères en dernier recours, après avoir effectué toutes les opérations nécessaires sur les données.

## Voir aussi

- [Documentation Rust sur les chaînes de caractères](https://doc.rust-lang.org/std/string/index.html)
- [Article sur les performances de la concaténation de chaînes en Rust](https://www.tourofrust.com/14_en.html)