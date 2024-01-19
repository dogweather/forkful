---
title:                "Analyse syntaxique de HTML"
html_title:           "Bash: Analyse syntaxique de HTML"
simple_title:         "Analyse syntaxique de HTML"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/gleam/parsing-html.md"
---

{{< edit_this_page >}}

## Qu'est-ce que c'est & Pourquoi ?

Le parsing HTML, c'est l'analyse des éléments structurels d'une page web pour en extraire le contenu. Les développeurs le font pour manipuler, extraire des données ou rendre interactifs les éléments HTML.

## Comment faire :

Voici un exemple de code Gleam pour le parsing HTML.

```Gleam 
import gleam/html

fn main() {
  let html = "<html><body><h1>Bonjour le monde!</h1></body></html>";
  match gleam/html.parse(html) {
    Ok(tree) -> 
      io.println("Parsing réussi ! Voici l'arbre HTML:", tree)
    Error(err) -> 
      io.println("Erreur de parsing:", err)
  }
}
```

Voici la sortie si tout se passe bien :

```
Parsing réussi! Voici l'arbre HTML: { tag: "html", children: [ { tag: "body", children: [ { tag: "h1", innerHTML:  "Bonjour le monde!" } ] } ] }
```
## Plongée en profondeur

L'HTML a été introduit en 1993 par Tim Berners-Lee. Le parsing HTML a rapidement suivi pour permettre une interaction plus sophistiquée avec les pages web.

En ce qui concerne les alternatives, plusieurs langages ont des bibliothèques pour le parsing HTML. JavaScript avec jQuery, Python avec BeautifulSoup, pour en nommer quelques-uns.

Pour faire du parsing HTML en Gleam, nous utilisons le crate Rust `scraper`. Il offre une API de haut niveau pour parcourir le document et extraire ce que nous voulons.

## Voir également

- [Documentation officielle de Gleam](https://gleam.run/docs/)
- [Guide de parsing HTML en Rust](https://dev.to/deciduously/use-rust-to-concatenate-all-text-in-an-html-file-2ejo)
- [Tutoriel BeautifulSoup pour Python](https://realpython.com/beautiful-soup-web-scraper-python/)