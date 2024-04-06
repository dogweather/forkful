---
date: 2024-01-20 18:01:11.226042-07:00
description: "Comment faire : L'authentification basique HTTP a \xE9t\xE9 une m\xE9\
  thode standard depuis les d\xE9buts du web pour prot\xE9ger les acc\xE8s. Cependant,\
  \ sa simplicit\xE9\u2026"
lastmod: '2024-04-05T22:51:11.403502-06:00'
model: gpt-4-1106-preview
summary: "L'authentification basique HTTP a \xE9t\xE9 une m\xE9thode standard depuis\
  \ les d\xE9buts du web pour prot\xE9ger les acc\xE8s."
title: "Envoi d'une requ\xEAte HTTP avec authentification de base"
weight: 45
---

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
