---
title:                "Analyse d'une date à partir d'une chaîne de caractères"
date:                  2024-01-20T15:38:39.167182-07:00
html_title:           "Arduino: Analyse d'une date à partir d'une chaîne de caractères"
simple_title:         "Analyse d'une date à partir d'une chaîne de caractères"
programming_language: "Rust"
category:             "Rust"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/rust/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## What & Why?
Parser une date depuis une chaîne de caractères permet de convertir du texte en une représentation structurée de date. On le fait pour traiter et analyser des dates dans des applications, comme des triages ou des comparaisons temporelles.

## How to:
Utilisons `chrono`, une crate Rust pour gérer le temps. D'abord, ajoutez-la à votre `Cargo.toml` :

```toml
[dependencies]
chrono = "0.4"
```

Maintenant, parsez une date avec ce bout de code :

```rust
extern crate chrono;
use chrono::{DateTime, NaiveDateTime, Utc, Local, TimeZone};

fn main() {
    let date_string = "2023-04-05T12:30:45Z";
    
    // Parsez une date/heure UTC
    let date_utc: DateTime<Utc> = date_string.parse().expect("Format de date non valide !");
    println!("Date/heure en UTC : {}", date_utc);
    
    // Parsez une date/heure locale
    let date_local: DateTime<Local> = DateTime::parse_from_rfc3339(date_string)
        .expect("Format de date non valide !")
        .with_timezone(&Local);
    println!("Date/heure locale : {}", date_local);
    
    // Parsez sans timezone (NaiveDateTime)
    let naive_date_format = "%Y-%m-%dT%H:%M:%S";
    let naive_date = NaiveDateTime::parse_from_str(date_string, naive_date_format)
        .expect("Format de date non valide !");
    println!("Date/heure naive : {}", naive_date);
}
```

Sortie typique :

```
Date/heure en UTC : 2023-04-05T12:30:45Z
Date/heure locale : 2023-04-05T14:30:45+02:00
Date/heure naive : 2023-04-05T12:30:45
```

## Deep Dive
Historiquement, Rust utilise `chrono` comme solution de facto pour la gestion du temps, bien que `std::time` existe aussi. `chrono` est plus riche en fonctionnalités, toutefois. Les alternatives, comme `time` ou utiliser `std::time` directement, exigent souvent une conversion manuelle et plus de code.

Pour le parsing, `chrono` supporte de multiples formats : RFC2822, RFC3339, ou des formats personnalisés. Les dates sans timezone sont manipulées avec `NaiveDateTime`, mais la plupart des applications préfèreront `DateTime` avec `Utc` ou `Local` pour s'assurer que toutes les heures sont bien traitées.

## See Also
- Le site de `chrono`: https://docs.rs/chrono/0.4.19/chrono/
- 'The Rust Programming Language' sur le traitement du temps : https://doc.rust-lang.org/book/ch10-02-traits.html
- RFC3339 et RFC2822 standards: https://www.ietf.org/rfc/rfc3339.txt, https://www.ietf.org/rfc/rfc2822.txt
