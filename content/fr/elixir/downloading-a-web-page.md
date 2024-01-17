---
title:                "Téléchargement d'une page web"
html_title:           "Elixir: Téléchargement d'une page web"
simple_title:         "Téléchargement d'une page web"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/elixir/downloading-a-web-page.md"
---

{{< edit_this_page >}}

# Qu'est-ce que c'est et pourquoi le faire ?
Télécharger une page web est essentiellement demander au navigateur de récupérer une page web à partir d'une adresse URL spécifique. Les développeurs le font souvent pour extraire des données à partir de sites web ou pour automatiser des tâches. 

## Comment faire :
```Elixir
requête = HTTPoison.get("https://www.example.com")
HTML.parse(requête.body)
```
Résultat :
```Elixir
{ :ok,
  %HTTPoison.Response{
    status_code: 200,
    body: "<html><head><title>Mon exemple de page web</title></head><body><p>Ceci est un exemple de page web.</p></body></html>",
    headers: [...],
    request_url: "https://www.example.com"
  }
}`
```

## Plongée en profondeur :
Le téléchargement de pages web a été rendu possible grâce à l'évolution des technologies web et des langages de programmation tels que Elixir. D'autres alternatives pour télécharger des pages web existent, comme l'utilisation de bibliothèques Python ou l'utilisation d'outils de scraping. L'implémentation de cette opération nécessite une bonne connaissance des requêtes HTTP et de la structure HTML.

## Voir aussi :
- Documentation sur la fonction HTTPoison : https://hexdocs.pm/httpoison/HTTPoison.html#get/3
- Tutoriel sur l'utilisation du HTTPoison : https://elixirschool.com/en/lessons/specifics/http/