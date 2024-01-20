---
title:                "Envoyer une requête http avec une authentification de base"
html_title:           "Arduino: Envoyer une requête http avec une authentification de base"
simple_title:         "Envoyer une requête http avec une authentification de base"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/clojure/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

# Clojure et les requêtes HTTP avec authentification de base
Nos mains sont déjà mouillées, plongées dans les bassins froids des codes. En avant pour les eaux profondes de Clojure, et voyons comment envoyer une requête HTTP avec authentification de base.

## Qu'est-ce que c'est & Pourquoi ?
L'envoi d'une requête HTTP avec authentification de base est un moyen courant pour un programme de passer des données à un serveur web, après avoir prouvé son identité. C'est une interaction sécurisée, souvent utilisée dans les API REST pour contrôler l'accès aux données sensibles.

## Comment faire :
Installons la bibliothèque HTTP "clj-http" avant de tremper nos orteils.

```clojure
;; Ajouter ce qui suit à votre fichier project.clj
[clj-http "3.12.2"]
```

Maintenant, envoyer une requête HTTP avec authentification de base :

```clojure
(require '[clj-http.client :as client])

(defn send-request []
  (let [url "https://your-api.com/retrieve-data"
        username "your-username"
        password "your-password"]
    (client/get url
                {:basic-auth [username password]})))

;;Il est temps de voir ce que notre requête renvoie.
(println (send-request))
```

Et voilà ! 

## Une plongée plus profonde
Historiquement, l'authentification de base était la première méthode d'authentification HTTP standard. Bien que son utilisation ait diminué en raison de ses limites de sécurité, elle est toujours courante dans les API REST.

Il y a des alternatives plus sécurisées. L'authentification par jeton est une option, elle utilise un jeton unique plutôt qu'un mot de passe pour authentifier un client.

Concernant les détails d'implémentation, clj-http fournit une abstraction haut niveau des requêtes HTTP. La clé :basic-auth prend un tableau de deux éléments, le nom d'utilisateur et le mot de passe, qui sont convertis en une chaîne codée en base64 et ajoutée à l'en-tête HTTP.

## Voir aussi
D'autres ressources pour affiner vos compétences en Clojure:

- [HTTP Basic Authentication, Mozilla Developer Network](https://developer.mozilla.org/fr/docs/Web/HTTP/Authentication#basic_authentication)
- Blog de [Martin Klepsch](https://martinklepsch.org) sur Clojure et clj-http