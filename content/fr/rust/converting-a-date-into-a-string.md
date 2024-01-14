---
title:                "Rust: Transformation d'une date en une chaîne de caractères."
simple_title:         "Transformation d'une date en une chaîne de caractères."
programming_language: "Rust"
category:             "Rust"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/rust/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Pourquoi

Si vous êtes nouveau dans le monde de la programmation Rust, vous vous demandez peut-être pourquoi il est important de savoir comment convertir une date en chaîne de caractères. Eh bien, la réponse est simple : la manipulation des dates est une tâche courante en programmation et comprendre comment effectuer cette tâche en Rust peut vous aider à créer des applications plus efficaces et fiables.

## Comment Faire

Pour convertir une date en chaîne de caractères en Rust, nous allons utiliser la bibliothèque standard "chrono". Tout d'abord, nous devons importer la bibliothèque en ajoutant cette ligne à notre code :

```Rust
use chrono::prelude::*;
```

Ensuite, nous pouvons créer un objet date en utilisant le type de données "Date" et en utilisant la méthode "from_ymd" en spécifiant l'année, le mois et le jour dans cet ordre :

```Rust
let date = Date::from_ymd(2021, 09, 08);
```

Maintenant, pour convertir cette date en chaîne de caractères, nous pouvons utiliser la méthode "format" qui prend en paramètre un format spécifique de date que nous voulons obtenir. Par exemple, si nous voulons la date au format "DD/MM/YYYY", nous pouvons utiliser cette ligne de code :

```Rust
let date_string = date.format("%d/%m/%Y").to_string();

```

Et voilà, nous avons maintenant notre date convertie en chaîne de caractères.

## Plongée Profonde

Pour ceux qui veulent en savoir plus sur la conversion des dates en chaînes de caractères en Rust, il est important de comprendre que la méthode "format" utilise en réalité le type de données "DateTime" qui comprend non seulement la date, mais aussi l'heure. Cela signifie que nous pouvons également formater notre chaîne de caractères pour inclure l'heure en utilisant les variables "hour", "minute" et "second" dans le format.

En outre, la bibliothèque "chrono" offre également d'autres fonctionnalités intéressantes pour la manipulation des dates, telles que la gestion des fuseaux horaires et la conversion entre différents formats de date. N'hésitez pas à explorer la documentation pour en savoir plus.

## Voir Aussi

- [Documentation de Chrono](https://docs.rs/chrono/0.4.19/chrono/)
- [Rust Cookbook pour la manipulation des dates](https://rust-lang-nursery.github.io/rust-cookbook/datetime.html)
- [Introduction à la manipulation des dates en Rust](https://thoughtbot.com/blog/manipulating-time-in-rust)