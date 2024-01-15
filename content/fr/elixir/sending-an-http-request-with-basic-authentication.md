---
title:                "Envoyer une requête http avec une authentification de base"
html_title:           "Elixir: Envoyer une requête http avec une authentification de base"
simple_title:         "Envoyer une requête http avec une authentification de base"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/elixir/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Pourquoi 
Sending an HTTP request with basic authentication is a necessary skill for any developer working with web applications. It allows you to securely send sensitive information, such as usernames and passwords, to a server for authentication.

## Comment faire
Pour envoyer une requête HTTP avec une authentification de base, vous pouvez utiliser la librairie HTTPoison disponible dans Elixir. D'abord, nous devons inclure cette librairie dans notre fichier en utilisant l'instruction `require`: 
```elixir
   require HTTPoison 
```
Ensuite, nous pouvons utiliser la fonction `HTTPoison.get/4` pour envoyer une requête avec notre nom d'utilisateur et mot de passe:
```elixir
   response = HTTPoison.get("https://example.com", %{
      basic_auth: {"username", "password"}
   })
```
Notez que nous avons passé notre nom d'utilisateur et mot de passe en tant que tuple dans l'option `basic_auth`. Maintenant, si la requête est réussie, la réponse contiendra les données renvoyées par le serveur.

## Plongée en profondeur
Lorsque nous envoyons une requête avec une authentification de base, notre nom d'utilisateur et mot de passe sont encodés en base64 avant d'être envoyés au serveur. Cela fait partie du protocole de base de l'authentification HTTP. De plus, nous pouvons également spécifier le type de contenu de la réponse que nous attendons en utilisant l'option `accept`:
```elixir
   response = HTTPoison.get("https://example.com", %{
      basic_auth: {"username", "password"},
      accept: "application/json"
   })
```
Cela indique au serveur que nous voulons une réponse au format JSON. Si le serveur peut fournir une telle réponse, elle sera convertie en une map Elixir dans notre réponse.

## Voir aussi
- [Documentation officielle HTTPoison](https://hexdocs.pm/httpoison/HTTPoison.html)
- [Tutoriel Elixir sur les requêtes HTTP](https://9elements.com/io/an-introduction-to-elixir/)
- [Article sur les différentes méthodes d'authentification HTTP](https://developer.okta.com/blog/2019/03/08/basic-authentication-with-examples)