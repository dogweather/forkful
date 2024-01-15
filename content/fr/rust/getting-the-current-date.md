---
title:                "Obtenir la date actuelle"
html_title:           "Rust: Obtenir la date actuelle"
simple_title:         "Obtenir la date actuelle"
programming_language: "Rust"
category:             "Rust"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/rust/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Pourquoi

Il y a plusieurs raisons pour lesquelles vous pourriez vouloir obtenir la date actuelle dans vos programmes Rust. Vous pourriez avoir besoin d'afficher la date sur une interface utilisateur, de sauvegarder des données avec une date de création ou même de déterminer le temps écoulé depuis une certaine date.

## Comment faire

Obtenir la date actuelle en Rust est assez simple. Tout d'abord, vous devez importer le module `time` :

```Rust
use time;
```

Ensuite, vous pouvez utiliser la méthode `now()` pour obtenir la date et l'heure actuelles dans le fuseau horaire local :

```Rust
let now = time::now();
println!("La date actuelle est : {}", now);
```

Vous pouvez également récupérer chaque composant individuel de la date comme suit :

```Rust
println!("Date : {}/{}/{}", now.tm_mday, now.tm_mon + 1, now.tm_year + 1900);
println!("Heure : {}:{}:{}", now.tm_hour, now.tm_min, now.tm_sec);
```

Et voici la sortie que vous obtiendrez en exécutant ces lignes de code :

```
La date actuelle est : Mon May 17 22:12:56 2021
Date : 17/5/2021
Heure : 22:12:56
```

## Plongeons plus profondément

Lorsque vous utilisez la méthode `now()`, vous obtenez un objet `Time`, qui contient toutes les informations sur la date et l'heure actuelles. Vous pouvez également spécifier un fuseau horaire différent en utilisant la méthode `now_utc()` ou `now_local()`.

De plus, il est possible d'effectuer des opérations sur les objets `Time`, comme l'ajout ou la soustraction d'une certaine quantité de temps. Par exemple, vous pouvez ajouter une heure à la date actuelle en utilisant la méthode `add_hours()` :

```Rust
let in_one_hour = now.add_hours(1);
println!("Dans une heure, il sera : {}", in_one_hour);
```

Il existe également d'autres méthodes utiles pour formater la date comme `strftime()` et `rfc3339()`, que vous pouvez découvrir dans la documentation officielle.

## Voir aussi

- [Documentation officielle pour le module `time`](https://docs.rs/time/latest/time/)
- [Exemples de formatage de date en Rust](https://docs.rs/time/latest/time/format/strftime/index.html)
- [Guide pour manipuler les dates et heures en Rust](https://blog.cloudflare.com/how-to-work-with-dates-and-times-in-rust/)