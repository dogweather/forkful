---
title:                "Télécharger une page web"
html_title:           "Bash: Télécharger une page web"
simple_title:         "Télécharger une page web"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/gleam/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Qu'est-ce que c'est & Pourquoi?

Télécharger une page web consiste à récupérer le code HTML d'une page spécifique sur Internet pour une utilisation hors ligne ou pour une analyse ultérieure. Les programmeurs le font pour récupérer des données, pour le web scrapping, ou pour mémoriser des sites web pour une utilisation ultérieure.

## Comment faire:

Gleam, étant un langage fonctionnel statiquement typé, ne dispose pas encore d'une bibliothèque standard pour le téléchargement de pages web. Toutefois, nous pouvons interagir avec Erlang ou Elixir, qui ont des bibliothèques HTTP robustes, pour accomplir cette tâche. Voici un exemple de code:

```Gleam
import gleam/httpc
import gleam/uri

fn download(url: String) {
  let parsed_url = uri.parse(url)
  case httpc.get(parsed_url) {
    Ok(#(status_code, headers, body)) ->
      body
    Error(e) ->
      e
  }
}
```

Pour tester cet exemple de code, vous pourriez utiliser un URL de test, comme `https://httpbin.org/get`, et vous devriez obtenir le corps de la page web en retour.

## Deep Dive

Historiquement, le téléchargement de pages web était la principale méthode par laquelle les contenus du web étaient accessibles hors ligne. Même aujourd'hui, cela reste une pratique courante pour le web scraping, l'archivage et, dans certains cas, le débogage.

Il existe de nombreuses alternatives à cette méthode, notamment l'utilisation d'API (tels que RESTful ou GraphQL) pour récupérer directement les données de la source, ou bien des bibliothèques tierces spécialisées dans le téléchargement et le traitement de pages web.

Le fonctionnement de cette fonction Gleam est assez simple. Il commence par analyser l'URL donnée en utilisant un parseur d'URI, puis il tente de récupérer les données à l'aide de la fonction `httpc.get`. Le corps de la réponse HTTP est ensuite retourné en cas de succès, et une erreur est retournée dans le cas contraire.

## Voir Aussi

4. [Autres ressources sur Erlang](https://www.erlang.org/docs)
5. [Documentation sur Elixir](https://hexdocs.pm/elixir/Kernel.html)