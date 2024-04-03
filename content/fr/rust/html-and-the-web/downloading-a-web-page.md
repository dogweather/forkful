---
date: 2024-01-20 17:44:54.734905-07:00
description: "How to: On utilise `reqwest`, une crate Rust populaire pour les requ\xEA\
  tes HTTP."
lastmod: '2024-03-13T22:44:57.480722-06:00'
model: gpt-4-1106-preview
summary: "On utilise `reqwest`, une crate Rust populaire pour les requ\xEAtes HTTP."
title: "T\xE9l\xE9chargement d'une page web"
weight: 42
---

## How to:
On utilise `reqwest`, une crate Rust populaire pour les requêtes HTTP.

```Rust
// Ajoutez d'abord `reqwest` et `tokio` à votre Cargo.toml
use reqwest;

#[tokio::main]
async fn main() -> Result<(), reqwest::Error> {
    let url = "http://example.com";
    let response = reqwest::get(url).await?;
    
    let contents = response.text().await?;
    println!("Le contenu de la page : {}", contents);
    
    Ok(())
}
```
Sortie (extrait) :
```
Le contenu de la page : <!doctype html> ...
```

## Deep Dive
Historiquement, télécharger une page web était un processus plus verbeux en Rust, souvent réalisé avec `hyper`, une autre crate HTTP. `reqwest` simplifie ça en encapsulant `hyper` et d'autres dépendances comme `tokio` pour l'asynchronisme.

Alternativement, `curl`, `wget`, ou `httpie` pourraient être utilisés en ligne de commande. Mais en Rust, `reqwest` offre un contrôle programmable et une intégration facile dans le code existant.

Concernant l'implémentation, `reqwest` utilise `async/await` de Rust pour traiter les requêtes en non-blocant, ce qui est essentiel pour la performance et l'efficacité lors du traitement simultané de multiples téléchargements.

## See Also
- Documentation `reqwest`: https://docs.rs/reqwest/
- Tutoriel `tokio` pour le traitement asynchrone: https://tokio.rs/tokio/tutorial
- Comparaison des crates pour les requêtes HTTP: https://www.arewewebyet.org/topics/http/
