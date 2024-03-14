---
date: 2024-01-20 17:43:39.282812-07:00
description: "T\xE9l\xE9charger une page web, c'est r\xE9cup\xE9rer son contenu via\
  \ le web. Les programmeurs le font pour analyser des donn\xE9es, tester des sites,\
  \ ou agr\xE9ger du\u2026"
lastmod: '2024-03-13T22:44:57.325741-06:00'
model: gpt-4-1106-preview
summary: "T\xE9l\xE9charger une page web, c'est r\xE9cup\xE9rer son contenu via le\
  \ web. Les programmeurs le font pour analyser des donn\xE9es, tester des sites,\
  \ ou agr\xE9ger du\u2026"
title: "T\xE9l\xE9chargement d'une page web"
---

{{< edit_this_page >}}

## What & Why? (Quoi & Pourquoi ?)
Télécharger une page web, c'est récupérer son contenu via le web. Les programmeurs le font pour analyser des données, tester des sites, ou agréger du contenu.

## How to: (Comment faire : )
Elixir, avec ses librairies comme HTTPoison, rend ça simple. Voici un exemple pour télécharger le contenu d'une page :

```elixir
# Ajoutez HTTPoison à votre mix.exs
defp deps do
  [
    {:httpoison, "~> 1.8"}
  ]
end

# Exemple basique de téléchargement d'une page
def download_page(url) do
  case HTTPoison.get(url) do
    {:ok, %HTTPoison.Response{status_code: 200, body: body}} ->
      body
    {:ok, %HTTPoison.Response{status_code: status_code}} ->
      "Erreur : #{status_code}"
    {:error, %HTTPoison.Error{reason: reason}} ->
      "Erreur : #{reason}"
  end
end

# Utilisation
IO.puts(download_page("http://example.com"))
```

Output attendu (cela peut varier selon le contenu actuel de la page) :

```
<!doctype html>
<html>
<head>
    <title>Example Domain</title>
...
```

## Deep Dive (Plongée en profondeur)
Historiquement, télécharger des données web se faisait en bash avec `curl` ou `wget`. En Elixir, HTTPoison s'appuie sur Hackney, une librairie robuste d'Erlang. Vous pourriez aussi utiliser Tesla, plus flexible avec des middlewares, ou même Scrapy en Python pour un scraping plus avancé. L'astuce avec Elixir, c'est de traiter les téléchargements de façon concurrente et efficace, grâce à la machine virtuelle Erlang.

## See Also (Voir aussi)
- [HTTPoison GitHub repository](https://github.com/edgurgel/httpoison)
- [Elixir Documentation](https://elixir-lang.org/docs.html)
- [Tesla: another HTTP client library for Elixir](https://github.com/teamon/tesla)
- [Scrapy: a powerful web scraping & crawling framework](https://scrapy.org/)
