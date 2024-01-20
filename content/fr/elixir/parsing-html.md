---
title:                "Analyse syntaxique de HTML"
date:                  2024-01-20T15:30:54.398780-07:00
html_title:           "Arduino: Analyse syntaxique de HTML"
simple_title:         "Analyse syntaxique de HTML"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/elixir/parsing-html.md"
---

{{< edit_this_page >}}

## What & Why? (Quoi et Pourquoi ?)
Analyser du HTML, c'est lire et comprendre le code d'une page web pour en extraire des informations spécifiques. Les programmeurs le font pour interagir avec des sites, collecter des données ou tester des applications web.

## How to: (Comment faire :) 
Avec Floki, une bibliothèque Elixir populaire pour l'analyse du HTML.

```elixir
# Ajoutez Floki à votre mix.exs
defp deps do
  [
    {:floki, "~> 0.34.0"}
  ]
end

# Exemple d'utilisation
defp extract_html_data(html) do
  # Parse the HTML
  {:ok, document} = Floki.parse_document(html)
  # Récupérez des éléments par leur sélecteur CSS
  titles = Floki.find(document, "h1.title")
  # Extrait le texte des éléments sélectionnés
  Enum.map(titles, &Floki.text/1)
end

html_content = """
<html>
<head><title>Test Page</title></head>
<body>
  <h1 class="title">Welcome to Elixir</h1>
  <h1 class="title">Parsing HTML with Floki</h1>
</body>
</html>
"""

IO.inspect extract_html_data(html_content)
# Devrait afficher: ["Welcome to Elixir", "Parsing HTML with Floki"]
```

## Deep Dive (Plongée en Profondeur)
Floki s'inspire de bibliothèques comme Nokogiri en Ruby et BeautifulSoup en Python. Avant ces outils, le parsing HTML était un cauchemar: fragile et sujet à erreur. Avec l'arrivée de HTML5 et des parseurs plus robustes, l'analyse de HTML est devenue plus prévisible. Floki utilise le parseur d'HTML5 mojic et s'appuie sur un modèle d'analyse basé sur des sélecteurs CSS pour extraire des données de manière concise.

## See Also (Voir Aussi)
- Floki sur Hex: [https://hex.pm/packages/floki](https://hex.pm/packages/floki)
- Guide officiel Elixir: [https://elixir-lang.org/getting-started/introduction.html](https://elixir-lang.org/getting-started/introduction.html)
- Documentation du parseur mojic: [https://github.com/mojic/](https://github.com/mojic/)