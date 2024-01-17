---
title:                "Envoi d'une requête http"
html_title:           "Javascript: Envoi d'une requête http"
simple_title:         "Envoi d'une requête http"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/javascript/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi?
L'envoi d'une requête HTTP est une action courante dans la programmation qui permet aux développeurs de récupérer des données depuis un serveur distant. Cela peut inclure des fichiers, des pages web, ou même des informations pour les applications mobiles. C'est une étape essentielle pour obtenir des informations en temps réel et améliorer l'expérience utilisateur.

## Comment faire: 
Voici un exemple simple de code Javascript pour envoyer une requête HTTP en utilisant la méthode GET:
```
fetch('https://monsite.com/donnees') // remplacez "monsite.com" par l'URL de votre choix
  .then(response => response.json()) // transforme la réponse en JSON
  .then(data => console.log(data)) // affiche les données récupérées dans la console
```
Output: Les données récupérées seront automatiquement affichées dans la console de votre navigateur.

## Plongée en profondeur: 
L'envoi de requêtes HTTP est un processus qui existe depuis les débuts du web. Aujourd'hui, il existe différentes méthodes et outils pour le faire, tels que fetch, XMLHttpRequest, et axios. Les développeurs peuvent également utiliser ces outils pour personnaliser les en-têtes de leurs requêtes, définir des paramètres et gérer les erreurs. Cependant, il est important de toujours vérifier si l'API que vous utilisez est sécurisée pour éviter toute exploitation.

## Voir aussi: 
- [MDN Web Docs: Fetch API](https://developer.mozilla.org/fr/docs/Web/API/Fetch_API)
- [Introduction à Axios pour les développeurs JavaScript](https://blog.logrocket.com/introduction-to-axios-for-developers/) 
- [XMLHttpRequest et Fetch API: les différences](https://www.opquast.com/blog/difference-entre-xmlhttprequest-et-fetch-api/)