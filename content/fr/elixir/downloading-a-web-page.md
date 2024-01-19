---
title:                "Télécharger une page web"
html_title:           "Bash: Télécharger une page web"
simple_title:         "Télécharger une page web"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/elixir/downloading-a-web-page.md"
---

{{< edit_this_page >}}

# Télécharger Une Page Web En Elixir: Un Guidage Simple et Clair

## C'est quoi et pourquoi?
Télécharger une page web signifie récupérer et stocker son contenu sur votre ordinateur. Les programmeurs le font pour analyser les informations, pour l'intégration de données, le scraping de sites web, les tests d'interface utilisateur, et bien plus.

## Comment faire:

Voici comment vous pouvez télécharger une page web en utilisant Elixir et la bibliothèque HTTPoison.

```Elixir
defmodule Download do
  def web_page(url) do
    case HTTPoison.get(url) do
      {:ok, %HTTPoison.Response{body: body}} ->
        {:ok, body}
      {:error, %HTTPoison.Error{reason: reason}} ->
        {:error, reason}
    end
  end
end
```

Utilisez-le comme ceci:

```Elixir
case Download.web_page("https://example.com/") do
  {:ok, body} -> IO.puts body
  {:error, reason} -> IO.puts "Erreur: #{reason}"
end
```
## Plongée en Profondeur

Télécharger des pages web a commencé dans les années 90 avec des navigateurs web primitifs. Aujourd'hui, c'est une méthode courante utilisée dans le développement web. En Elixir, il existe plusieurs bibliothèques pour le faire, dont HTTPoison et HTTPotion. 

HTTPoison est la plus populaire car elle fournit une interface simple pour effectuer des requêtes HTTP. Son principal avantage est sa facilité d'utilisation combinée à sa flexibilité. Elle permet des requêtes HTTP asynchrones, ce qui peut considérablement accélérer les opérations de téléchargement en parallèle.

L'implémentation ci-dessus effectue une requête GET pour récupérer le contenu de la page. En cas de succès, elle retourne le corps de la réponse, sinon elle retourne l'erreur.

## A Voir Aussi

Pour plus d'informations, consultez les liens suivants:

- [HTTPoison sur Github](https://github.com/edgurgel/httpoison)
- [HTTPotion sur Github](https://github.com/myfreeweb/httpotion)
- [Guide Elixir officiel](https://elixir-lang.org/getting-started/introduction.html)