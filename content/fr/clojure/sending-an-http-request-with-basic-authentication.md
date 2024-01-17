---
title:                "Envoyer une requête http avec une authentification de base"
html_title:           "Clojure: Envoyer une requête http avec une authentification de base"
simple_title:         "Envoyer une requête http avec une authentification de base"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/clojure/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

# Quoi & Pourquoi?

Envoyer une requête HTTP avec une authentification de base est un moyen pour les programmeurs d'envoyer des informations à un serveur via un protocole de communication web standard. Cela permet de sécuriser les échanges de données entre un client et un serveur en demandant une vérification d'identité pour accéder à des ressources protégées.

# Comment faire:

```Clojure
(require '[clj-http.client :as client]) 

(client/get "https://www.example.com" 
  :basic-auth "username" "password" 
  :as :string)
```

En utilisant la bibliothèque clj-http, nous pouvons facilement envoyer une requête HTTP avec une authentification de base en spécifiant un nom d'utilisateur et un mot de passe. Nous obtenons ainsi une réponse sous forme de chaîne de caractères en utilisant l'option :as :string dans la requête.

# Deep Dive:

Historiquement, l'authentification de base a été développée pour permettre aux utilisateurs d'accéder à des ressources en ligne en utilisant des informations d'identification simples. Cependant, il est considéré comme un moyen de sécurité faible car les informations d'identification sont envoyées en clair lors de chaque requête.

Une alternative plus sécurisée à l'authentification de base est l'authentification par jeton où un jeton d'accès unique est utilisé à la place des informations d'identification. Cela permet une connexion unique et sécurisée, sans avoir à envoyer des informations d'identification à chaque requête.

Dans l'exemple de code ci-dessus, nous utilisons la bibliothèque clj-http qui gère les détails de mise en œuvre de l'authentification de base. Cependant, nous pouvons également implémenter manuellement une requête HTTP avec une authentification de base en ajoutant un en-tête d'authentification contenant le nom d'utilisateur et le mot de passe codé en base64.

# Voir aussi:

- La documentation de la bibliothèque clj-http : https://github.com/dakrone/clj-http 
- Un article sur l'authentification de base : https://developer.mozilla.org/fr/docs/Web/HTTP/Authentication 
- Un exemple d'implémentation manuelle : https://stackoverflow.com/questions/21683973/send-http-request-with-basic-authentication-in-clojure