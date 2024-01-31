---
title:                "Envoi d'une requête HTTP avec authentification de base"
date:                  2024-01-20T18:01:11.226042-07:00
model:                 gpt-4-1106-preview
simple_title:         "Envoi d'une requête HTTP avec authentification de base"

category:             "Clojure"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/clojure/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?
Envoyer une requête HTTP avec une authentification basique transmet des identifiants d’accès à un serveur web sécurisé. Les développeurs utilisent cela pour accéder aux ressources protégées via des API ou des services web.

## Comment faire :
```Clojure
(require '[clj-http.client :as client])

(defn send-authenticated-request []
  (let [url "http://example.com/api/resource"
        credentials (str "Basic " (.encode (java.util.Base64/getEncoder) (.getBytes "user:password")))]
    (:body
     (client/get url {:headers {"Authorization" credentials}}))))

(println (send-authenticated-request))
```
Exemple de sortie :
```Clojure
"{\"data\": {\"id\": 1, \"name\": \"Un exemple de ressource\"}}"
```

## Deep Dive
L'authentification basique HTTP a été une méthode standard depuis les débuts du web pour protéger les accès. Cependant, sa simplicité engendre des faiblesses : les identifiants sont simplement encodés en base64, sans chiffrement. Alternativement, l'authentification par jeton, comme OAuth, offre une sécurité renforcée. Pour implémenter une requête authentifiée en Clojure, on utilise souvent `clj-http`, qui simplifie le processus. L'essentiel est de construire une chaîne de caractères encodée en base64 à partir de "utilisateur:mot de passe" puis de l'ajouter dans l'en-tête de la requête.

## Voir aussi
- La documentation officielle de `clj-http` pour plus de détails : [https://github.com/dakrone/clj-http](https://github.com/dakrone/clj-http)
- Une introduction à l'authentification HTTP de base sur MDN : [https://developer.mozilla.org/fr/docs/Web/HTTP/Authentication](https://developer.mozilla.org/fr/docs/Web/HTTP/Authentication)
- Plus d'informations sur les alternatives d'authentification sécurisées comme OAuth : [https://oauth.net/](https://oauth.net/)
