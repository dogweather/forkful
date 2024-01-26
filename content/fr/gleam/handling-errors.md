---
title:                "Gestion des erreurs"
date:                  2024-01-26T00:52:29.259861-07:00
model:                 gpt-4-1106-preview
simple_title:         "Gestion des erreurs"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/gleam/handling-errors.md"
---

{{< edit_this_page >}}

## Quoi et Pourquoi ?
La gestion des erreurs consiste à anticiper les dysfonctionnements dans votre code et à gérer ces situations avec élégance. Les programmeurs font cela car cela rend les applications robustes et conviviales, même en cas d'imprévus.

## Comment faire :
Dans Gleam, vous utiliserez souvent le type `Result` pour la gestion des erreurs. C'est une énumération avec deux variantes : `Ok` (pour succès) et `Error` (pour échec). Voici un exemple simple :

```Gleam
pub fn might_fail(break_it: Bool) -> Result(Int, String) {
  if break_it {
    Error("Oops! Ça a cassé.".to_string())
  } else {
    Ok(42)
  }
}

pub fn main() {
  let result = might_fail(False)
  case result {
    Ok(value) => value
    Error(message) => {
      io.println(message)
      0
    } 
  }
}
```

Si vous exécutez `main` avec `might_fail(False)`, cela retournera `42`. Si vous passez `True`, cela affiche "Oops! Ça a cassé." et retourne `0`.

## Exploration Approfondie
L'approche de Gleam pour la gestion des erreurs est influencée par ses origines Erlang. Historiquement, Erlang utilise une philosophie "laissez-le se planter", en s'appuyant sur des arbres de supervision pour gérer les échecs des processus. Cependant, lorsque vous écrivez du code Gleam qui n'est pas à l'intérieur d'un processus destiné à être supervisé, comme dans une fonction de bibliothèque, vous voudrez gérer explicitement les erreurs.

Les alternatives à l'utilisation de `Result` incluent l'utilisation du type `Option` pour les cas où quelque chose pourrait être `None` (rien) ou `Some` (quelque chose), mais ceux-ci ne portent pas d'informations sur les erreurs. Pour signaler des erreurs au-delà des limites des processus, vous pourriez utiliser les mécanismes de passage de messages d'Erlang.

La gestion des erreurs dans Gleam reflète un style de programmation fonctionnelle, où les effets secondaires (comme les erreurs) sont gérés avec des types et des correspondances de motifs, offrant une clarté et une prévisibilité dans la gestion des erreurs.

## Voir Aussi
- [La Gestion des Erreurs en Erlang](http://erlang.org/doc/reference_manual/errors.html)