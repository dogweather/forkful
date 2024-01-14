---
title:    "Rust: Obtenir la date actuelle"
keywords: ["Rust"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/rust/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Pourquoi
Si vous êtes un développeur qui apprend Rust ou si vous êtes déjà familier avec le langage, vous savez probablement à quel point il est important de maîtriser la manipulation des dates et heures. Dans cet article, nous allons explorer comment obtenir la date et l'heure actuelles en utilisant Rust.

## Comment faire
Voici comment vous pouvez obtenir la date et l'heure actuelles en utilisant Rust :

```Rust
use std::time::{SystemTime, UNIX_EPOCH};

let now = SystemTime::now();
let since_epoch = now.duration_since(UNIX_EPOCH).expect("Le composant horodaté lu est antérieur à l'époque UNIX.");

println!("Il s'est écoulé {:?} secondes depuis l'époque UNIX.", since_epoch);
```

Output :

```
Il s'est écoulé Duration { secs: 1638581007, nanos: 810074000 } secondes depuis l'époque UNIX.
```

Comme vous pouvez le voir dans cet exemple, nous utilisons la struct `SystemTime` pour obtenir l'heure actuelle et la comparons avec l'époque UNIX en utilisant la méthode `duration_since()`. Vous pouvez également transformer cette durée en d'autres formats si vous le souhaitez.

## Deep Dive
Il est important de noter que `SystemTime` n'est pas spécifiquement liée à l'époque UNIX, c'est-à-dire le 1er janvier 1970 à 00:00:00 UTC. Cependant, il est couramment utilisé pour représenter les dates et heures en informatique. De plus, certaines plates-formes peuvent utiliser différents points de départ pour leur mesure du temps, donc gardez toujours cela à l'esprit lorsque vous travaillez avec des dates et heures.

## Voir aussi
- [Documentation officielle de Rust sur la manipulation des dates et heures](https://doc.rust-lang.org/std/time/index.html)
- [Article de blog sur la conversion des dates en Rust](https://sapphirepaw.github.io/AIRRS/manipulating-time-in-rust/)