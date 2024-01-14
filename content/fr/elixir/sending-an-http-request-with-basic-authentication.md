---
title:                "Elixir: Envoi d'une requête http avec une authentification de base"
simple_title:         "Envoi d'une requête http avec une authentification de base"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/elixir/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Pourquoi
Les requêtes HTTP avec une authentification de base sont couramment utilisées dans le développement web pour permettre aux utilisateurs d'accéder à des ressources protégées. Cela peut être utile lorsque l'on souhaite limiter l'accès à certaines informations sensibles ou lorsque l'on veut restreindre l'utilisation d'une API. Dans cet article, nous allons explorer comment effectuer une telle requête en utilisant Elixir.

## Comment faire
Pour envoyer une requête HTTP avec une authentification de base en utilisant Elixir, nous allons d'abord devoir importer le module `HTTPoison` dans notre code :

```Elixir
alias HTTPoison.Client, as: Http
```

Ensuite, nous pouvons utiliser la fonction `post` du module `HTTPoison` pour envoyer une requête avec l'authentification de base. Voici un exemple de code pour envoyer une requête POST avec un nom d'utilisateur et un mot de passe :

```Elixir
url = "http://example.com/api/resource"
payload = %{some_data: "some_value"}
auth = {username: "user123", password: "p@ssw0rd"}
response = Http.post(url, {payload, [], auth})
```

L'exemple ci-dessus utilise un `url` cible, un `payload` à envoyer et une paire d'identifiants `auth` pour l'authentification de base. Si la requête est réussie, la réponse contiendra le code de statut HTTP ainsi que le corps de la réponse. Voici un aperçu du contenu de la réponse :

```Elixir
%HTTPoison.Response{
  body: "Contenu de la réponse",
  headers: %{},
  status_code: 200
}
```

## Profondeur de plongée
Maintenant que nous avons vu comment envoyer une requête HTTP avec une authentification de base en utilisant Elixir, il est important de comprendre le processus derrière cela. Lorsque nous utilisons la fonction `post` du module `HTTPoison`, elle utilise en fait les fonctionnalités du module `HTTPc` pour établir une connexion avec le serveur et envoyer la requête en utilisant le protocole HTTP.

Le paramètre d'authentification que nous avons utilisé est en fait une liste de tuples contenant les informations d'identification pour l'authentification de base. Ces informations sont ensuite encodées en utilisant l'algorithme Base64 avant d'être envoyées comme en-tête dans la requête HTTP.

Il est également important de noter que les requêtes avec une authentification de base sont considérées comme moins sécurisées que les autres méthodes d'authentification, car les données sont envoyées en texte brut et peuvent être facilement lues par des tiers. Il est donc recommandé d'utiliser d'autres méthodes d'authentification pour les scénarios où la sécurité est une préoccupation majeure.

## Voir aussi
- [Documentation du module HTTPoison](https://hexdocs.pm/httpoison/HTTPoison.html)
- [Documentation du module HTTPc](https://hexdocs.pm/httpc/HTTPc.html)
- [Article sur la sécurité des requêtes HTTP](https://www.thoughtco.com/why-basic-auth-sucks-756572)