---
title:                "Analyse de html"
html_title:           "Elixir: Analyse de html"
simple_title:         "Analyse de html"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/elixir/parsing-html.md"
---

{{< edit_this_page >}}

# Que & Pourquoi?

Le parsing HTML est le processus d'analyse d'un document HTML afin de le convertir en une structure de données que les ordinateurs peuvent comprendre et utiliser. Les programmeurs le font pour traiter les données web de manière efficace et automatisée.

# Comment faire:

Utilisez le module "Floki" dans Elixir pour effectuer le parsing HTML. Voici un exemple simple pour extraire le titre d'une page Web:

```
html = "<html><head><title>Mon premier article</title></head><body><h1>Titre principal</h1><p>C'est mon premier article sur Elixir!</p></body></html>"

Floki.find(html, "title")
```

Output:
```
["Mon premier article"]
```

Vous pouvez également utiliser la syntaxe CSS pour cibler des éléments spécifiques sur une page:

```
Floki.find(html, "h1")
```

Output:
```
["Titre principal"]
```

# Exploration approfondie:

Le parsing HTML a été introduit pour la première fois en 1967 par Robert F. Cailliau et Tim Berners-Lee dans le but de faciliter l'échange de documents sur Internet. Il existe d'autres méthodes de parsing, telles que le DOM parsing et le SAX parsing, mais le parsing HTML est le plus couramment utilisé.

Il existe également des librairies externes telles que "Elixir-HTML" et "Meeseeks", qui offrent des fonctionnalités supplémentaires telles que la validation HTML et la manipulation de documents HTML.

En termes d'implémentation, le module "Floki" utilise un algorithme appelé "sibling-searching" pour parcourir l'arborescence du document en suivant les relations frères-sœurs. Cela rend le parsing plus efficace et rapide que d'autres méthodes.

# Voir aussi:

- Documentation officielle de Floki: https://hexdocs.pm/floki/api-reference.html
- Elixir-HTML: https://github.com/bryanjos/elixir-html
- Meeseeks: https://github.com/artemeff/meeseeks