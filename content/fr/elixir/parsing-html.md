---
title:                "Analyse syntaxique de HTML"
html_title:           "Bash: Analyse syntaxique de HTML"
simple_title:         "Analyse syntaxique de HTML"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/elixir/parsing-html.md"
---

{{< edit_this_page >}}

## Quoi et Pourquoi?
Le _parsing_ HTML, c'est le processus d'analyse d'un document HTML pour le décomposer en éléments exploitables par la programmation. Les programmeurs le font car cela leur permet d'extraire, de manipuler et de traiter les données présentes dans des documents HTML.

## Comment ça marche:
Voici une façon simple de faire du parsing HTML en Elixir en utilisant la bibliothèque `Floki`. 

Tout d'abord, installez Floki via mix.exs:

```elixir
defp deps do
  [
    {:floki, "~> 0.30.0"}
  ]
end
```

Et puis lancez `mix deps.get` dans le terminal pour télécharger la dépendance.

Ensuite, utilisez Floki pour parse un HTML:

```elixir
html = "<html><body><h1>Salut le monde!</h1></body></html>"
{:ok, parsed_html} = Floki.parse_document(html)
IO.inspect(parsed_html)
```

L'output serait en forme d'un arbre structuré:

```elixir
[{"html", [],
  [{"body", [],
    [{"h1", [], ["Salut le monde!"]}]}]}]
```

## Approfondissement 
Historiquement, le parsing HTML a été un défi à cause de la nature souvent désorganisée des documents HTML. La bibliothèque Floki facilite cette tâche en Elixir.

Il existe bien sûr des alternatives à Floki, comme `meeseeks` et `mochiweb_html`, mais Floki est reconnu pour sa simplicité d'utilisation.

Du point de vue de l'implémentation, Floki fonctionne en décomposant un document HTML en une structure arborescente appelée Document Object Model (DOM), qui peut ensuite être parcourue de manière programmatique.

## Et Aussi
Pour continuer à approfondir vos connaissances du parsing HTML en Elixir, jetez un œil à ces ressources:

- Floki sur Hex: [https://hexdocs.pm/floki](https://hexdocs.pm/floki)
- Autres parseurs HTML en Elixir: 
  - Meeseeks: [https://hexdocs.pm/meeseeks/Meeseeks.html](https://hexdocs.pm/meeseeks/Meeseeks.html)
  - Mochiweb_HTML: [https://github.com/mochi/mochiweb](https://github.com/mochi/mochiweb)
- Tutoriels Elixir supplémentaires: [https://elixirschool.com/fr/](https://elixirschool.com/fr/)