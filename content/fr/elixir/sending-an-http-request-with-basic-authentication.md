---
title:                "Envoi d'une requête http avec une authentification de base"
html_title:           "Elixir: Envoi d'une requête http avec une authentification de base"
simple_title:         "Envoi d'une requête http avec une authentification de base"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/elixir/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi?

Envoyer une requête HTTP avec une authentification de base est une façon sécurisée d'accéder à des données sur un serveur distant. Les programmeurs utilisent cette méthode lorsqu'ils ont besoin d'échanger des informations sensibles entre deux systèmes.

## Comment faire:

Voici un exemple de code pour envoyer une requête HTTP avec une authentification de base en utilisant Elixir:

```Elixir
require HTTPoison
HTTPoison.get("https://exemple.com/api", [], [basic_auth: {"utilisateur", "mot de passe"}])
```

Output: Le serveur distant retournera la réponse attendue pour la requête, mais avec une connexion sécurisée grâce à l'authentification de base.

## Plongée en profondeur:

L'authentification de base est un protocole de sécurité de base pour accéder à des ressources en ligne. Il a été introduit dans les années 1990 et fonctionne en envoyant un nom d'utilisateur et un mot de passe en clair à chaque requête. Bien que cette méthode soit considérée comme moins sécurisée que d'autres formes d'authentification comme OAuth, elle est toujours largement utilisée en raison de sa simplicité et de sa compatibilité avec de nombreux serveurs.

Alternativement, les programmeurs peuvent utiliser des méthodes plus avancées comme l'authentification via token ou l'utilisation d'une clé API pour sécuriser leurs requêtes HTTP.

## Voir Aussi:

Pour en savoir plus sur l'authentification de base et les alternatives, vous pouvez consulter les ressources suivantes:
- [RFC pour l'authentification de base](https://tools.ietf.org/html/rfc2617)
- [Comparaison des différentes méthodes d'authentification pour les requêtes HTTP](https://www.topcoder.com/blog/understanding-oauth-vs-basic-authentication/)