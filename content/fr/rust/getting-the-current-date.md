---
title:    "Rust: Obtenir la date actuelle"
keywords: ["Rust"]
---

{{< edit_this_page >}}

## Pourquoi

La date et l'heure sont des éléments essentiels dans de nombreuses applications et programmes. Que ce soit pour afficher la date actuelle sur un calendrier, enregistrer le moment de la dernière mise à jour d'un fichier, ou même planifier des tâches à exécuter à une heure précise, il est souvent nécessaire de pouvoir accéder à la date et l'heure actuelles dans un programme. Dans cet article, nous allons expliquer comment obtenir la date actuelle en utilisant le langage de programmation Rust.

## Comment faire

Pour obtenir la date actuelle en Rust, nous allons utiliser la bibliothèque standard `std::time` et la fonction `now()`. Voici un exemple de code qui affiche la date et l'heure actuelles :

```Rust
use std::time;

fn main() {
    let now = time::now();
    println!("La date actuelle est : {}", now);
}
```

Lorsque nous exécutons ce code, nous obtenons la sortie suivante :

```
La date actuelle est : 2020-11-07 10:15:48.42708 +0000
```

Nous pouvons également formater la date selon nos préférences en utilisant la fonction `strftime()` et un string de format. Voici un exemple qui affiche la date au format JJ/MM/AAAA :

```Rust
use std::time;
use std::time::strftime;

fn main() {
    let now = time::now();
    let formatted_date = strftime("%m/%d/%Y", &now).unwrap();
    println!("La date actuelle est : {}", formatted_date);
}
```

La sortie sera maintenant :

```
La date actuelle est : 11/07/2020
```

## Plongée profonde

La fonction `now()` de la bibliothèque `std::time` renvoie un type `time::Tm` qui représente une date et une heure. Ce type a plusieurs méthodes utiles, comme `year()`, `month()`, `day()`, `hour()`, `minute()`, `second()`, qui nous permettent d'accéder à chaque élément de la date.

De plus, `std::time` contient également la struct `SystemTime` qui peut être utilisée pour représenter un point dans le temps absolue, sans dépendre du fuseau horaire. Cette struct a la méthode `now()` qui peut être utilisée pour obtenir l'heure système actuelle.

Il existe également des bibliothèques de Rust tierces spécialement conçues pour la manipulation de date et d'heure, telles que `chrono` ou `date_time`, qui offrent des fonctionnalités plus avancées.

## Voir aussi

Pour en apprendre davantage sur la manipulation de date et d'heure en Rust, voici quelques ressources supplémentaires :

- [Documentation officielle de Rust sur std::time](https://doc.rust-lang.org/std/time/)
- [Bibliothèque chrono](https://crates.io/crates/chrono)
- [Bibliothèque date_time](https://crates.io/crates/date_time)