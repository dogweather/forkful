---
title:                "Convertir une date en chaîne de caractères"
html_title:           "Rust: Convertir une date en chaîne de caractères"
simple_title:         "Convertir une date en chaîne de caractères"
programming_language: "Rust"
category:             "Rust"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/rust/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

# Rust: Convertir une date en chaîne de caractères

## Quoi & Pourquoi?

Convertir une date en chaîne de caractères est le processus de transformer une date en une forme lisible pour les humains. Les programmeurs font souvent cela lorsqu'ils ont besoin d'afficher une date pour les utilisateurs finaux ou lorsqu'ils doivent stocker une date dans une base de données en tant que chaîne de caractères.

## Comment faire:

Voici un exemple de code montrant comment convertir une date en chaîne de caractères en utilisant la bibliothèque standard de Rust. Nous allons utiliser la fonction `format!` pour formater notre date dans le format souhaité puis la convertir en chaîne de caractères à l'aide de la méthode `to_string()` :

```rust
use std::time::SystemTime;

fn main() {
    let date = SystemTime::now();

    let formatted_date = format!("{}", date);

    let date_string = formatted_date.to_string();

    println!("La date actuelle est: {}", date_string);
}
```

Cet exemple utilise la date actuelle en tant que point de départ, mais vous pouvez également spécifier une date spécifique en utilisant la structure `SystemTime` et en utilisant la méthode `now()` pour obtenir la date actuelle.

Voici la sortie attendue pour cet exemple :

```
La date actuelle est: Sun Sep 05 12:01:34 2021
```

## Plongée en profondeur:

Historiquement, la conversion de dates en chaînes de caractères a été un sujet difficile pour les programmeurs, car cela impliquait souvent de manipuler manuellement le format de la date et de la convertir en une chaîne de caractères. Mais avec l'avènement de bibliothèques modernes telles que `datetime` en Python ou le module `chrono` en Rust, cette tâche est devenue beaucoup plus simple.

Une alternative à la conversion de dates en chaînes de caractères est de les stocker sous forme de nombres entiers dans une base de données, ce qui peut être plus efficace. Cependant, cela nécessite de convertir ces nombres en dates lisibles pour les utilisateurs finaux lors de l'affichage.

En termes d'implémentation, les langages de programmation modernes, y compris Rust, ont des bibliothèques puissantes qui prennent en charge la conversion de dates en chaînes de caractères de manière efficace et facile à utiliser.

## Voir aussi:

- [Documentation sur la bibliothèque standard de Rust pour les dates](https://doc.rust-lang.org/std/time/struct.SystemTime.html)
- [Exemple d'utilisation de la bibliothèque `chrono` en Rust](https://docs.rs/chrono/0.4.19/chrono/)
- [Article sur la conversion de dates en chaînes de caractères en Python](https://realpython.com/python-string-formatting/#2-the-old-way-using-the-format-method)