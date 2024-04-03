---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:11:49.791565-07:00
description: "L'analyse de HTML en Elixir consiste \xE0 extraire des informations\
  \ des documents HTML. Les programmeurs font cela pour interagir de mani\xE8re programmatique\u2026"
lastmod: '2024-03-13T22:44:57.324626-06:00'
model: gpt-4-0125-preview
summary: "L'analyse de HTML en Elixir consiste \xE0 extraire des informations des\
  \ documents HTML."
title: Analyse Syntaxique du HTML
weight: 43
---

## Comment faire :
Elixir, avec son modèle robuste de concurrence et son paradigme de programmation fonctionnelle, n'inclut pas de capacités d'analyse HTML intégrées. Cependant, vous pouvez utiliser des bibliothèques tierces populaires comme `Floki` à cet effet. Floki rend l'analyse de HTML intuitive et efficace, en tirant parti des fonctionnalités de correspondance de motifs et de mise en pipeline d'Elixir.

Tout d'abord, ajoutez Floki à vos dépendances dans mix.exs :

```elixir
defp deps do
  [
    {:floki, "~> 0.31.0"}
  ]
end
```

Ensuite, exécutez `mix deps.get` pour installer la nouvelle dépendance.

Maintenant, analysons une simple chaîne HTML pour extraire des données. Nous rechercherons les titres à l'intérieur des balises `<h1>` :

```elixir
html_content = """
<html>
  <body>
    <h1>Bonjour, Elixir !</h1>
    <h1>Un Autre Titre</h1>
  </body>
</html>
"""

titles = html_content
         |> Floki.find("h1")
         |> Floki.text()

IO.inspect(titles)
```

**Exemple de sortie :**

```elixir
["Bonjour, Elixir !", "Un Autre Titre"]
```

Pour approfondir, disons que vous souhaitez extraire des liens (balises `<a>`) ainsi que leurs attributs href. Voici comment vous pouvez y parvenir :

```elixir
html_content = """
<html>
  <body>
    <a href="https://elixir-lang.org/">Site Officiel d'Elixir</a>
    <a href="https://hexdocs.pm/">HexDocs</a>
  </body>
</html>
"""

links = html_content
        |> Floki.find("a")
        |> Enum.map(fn({_, attrs, [text]}) -> {text, List.keyfind(attrs, "href", 0)} end)
        
IO.inspect(links)
```

**Exemple de sortie :**

```elixir
[{"Site Officiel d'Elixir", {"href", "https://elixir-lang.org/"}}, {"HexDocs", {"href", "https://hexdocs.pm/"}}]
```

Cette approche vous permet de naviguer et d'analyser efficacement les documents HTML, rendant les tâches d'extraction et de manipulation de données web simples dans les applications Elixir.
