---
title:                "Envoyer une demande http"
html_title:           "Gleam: Envoyer une demande http"
simple_title:         "Envoyer une demande http"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/gleam/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Qu'est-ce que c'est & pourquoi le faire?

L'envoi d'une requête HTTP est un moyen pour les programmeurs de communiquer avec des serveurs via le protocole HTTP. Cela leur permet de récupérer des données à partir de pages Web ou de services en ligne, ou d'envoyer des données pour y être traitées. Il est couramment utilisé dans le développement Web pour créer des sites Web interactifs et dynamiques.

## Comment faire:

```
Gleam.http.request(
	{
	  method: "GET",
	  url: "https://example.com",
	  headers: [ {"content-type", "application/json"} ],
	  body: "This is the request body"
	}
) `

```Gleam.http.request``` est la fonction clé pour envoyer une requête HTTP avec Gleam. Dans cet exemple, nous demandons à l'API de exmaple.com d'envoyer une requête GET avec un en-tête JSON et un corps de requête contenant du texte. Les données renvoyées seront stockées dans une variable que vous pouvez ensuite utiliser pour effectuer des opérations supplémentaires.

## Plongeon en profondeur:

L'envoi de requêtes HTTP est une pratique courante dans le développement Web depuis les années 1990 et reste une méthode populaire pour communiquer avec des serveurs. Alternativement, certaines applications Web modernes peuvent utiliser des technologies telles que GraphQL pour gérer leurs requêtes. Dans Gleam, l'implémentation de cette fonctionnalité repose sur les bibliothèques standard Erlang et les bibliothèques communautaires.

## Voir aussi:

- La documentation officielle de Gleam pour en savoir plus sur la fonction ```Gleam.http.request```.
- La liste des bibliothèques communautaires pour le développement Web en Gleam.
- Un article sur la différence entre HTTP et HTTPS.