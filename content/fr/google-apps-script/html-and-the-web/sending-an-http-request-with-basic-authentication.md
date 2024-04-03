---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:02:01.336403-07:00
description: "L'envoi d'une requ\xEAte HTTP avec une authentification de base consiste\
  \ \xE0 coder un nom d'utilisateur et un mot de passe dans un en-t\xEAte de requ\xEA\
  te pour\u2026"
lastmod: '2024-03-13T22:44:57.185126-06:00'
model: gpt-4-0125-preview
summary: "L'envoi d'une requ\xEAte HTTP avec une authentification de base consiste\
  \ \xE0 coder un nom d'utilisateur et un mot de passe dans un en-t\xEAte de requ\xEA\
  te pour acc\xE9der \xE0 des ressources prot\xE9g\xE9es."
title: "Envoyer une requ\xEAte HTTP avec une authentification de base"
weight: 45
---

## Comment faire :
Dans Google Apps Script, pour envoyer une requête HTTP avec authentification de base, vous utilisez le service `UrlFetchApp` combiné à un en-tête d'autorisation codé en base64. Voici un guide étape par étape :

1. **Encoder les identifiants** : Tout d'abord, encodez votre nom d'utilisateur et votre mot de passe en base64. Google Apps Script n'a pas de fonction native de codage en base64 pour les chaînes de caractères, vous utiliserez donc Utilities.base64Encode à cet effet.

```javascript
var username = 'VotreNomDUtilisateur';
var password = 'VotreMotDePasse';
var encodedCredentials = Utilities.base64Encode(username + ':' + password);
```

2. **Configurer les options de la requête** : Avec les identifiants encodés prêts, préparez l'objet options pour la requête HTTP, y compris la méthode et les en-têtes.

```javascript
var options = {
  method: 'get', // ou 'post', 'put', selon vos besoins
  headers: {
    'Authorization': 'Basic ' + encodedCredentials
  }
  // des options supplémentaires comme 'muteHttpExceptions' pour la gestion des erreurs peuvent être ajoutées ici
};
```

3. **Faire la requête** : Utilisez la méthode `UrlFetchApp.fetch` avec l'URL cible et l'objet options.

```javascript
var url = 'https://example.com/api/resource';
var response = UrlFetchApp.fetch(url, options);
Logger.log(response.getContentText());
```

L'exemple de sortie pour une requête réussie variera en fonction de la réponse de l'API. Pour une API basée sur JSON, vous pourriez voir quelque chose comme :

```
{"status":"Success","data":"Données de la ressource ici..."}
```

Assurez-vous de gérer les éventuelles erreurs HTTP en vérifiant le code de réponse ou en utilisant l'option `muteHttpExceptions` pour une gestion des erreurs plus contrôlée.

## Approfondissement
L'envoi d'une requête HTTP avec authentification de base a été une méthode standard dans de nombreux langages de programmation pour accéder aux ressources Web qui nécessitent une authentification. Dans le contexte de Google Apps Script, `UrlFetchApp` offre un moyen simple d'effectuer ces requêtes HTTP, y compris celles nécessitant une authentification. L'inclusion des identifiants de base dans les en-têtes de requête est une méthode simple mais efficace, mais elle comporte des mises en garde de sécurité, principalement parce que les identifiants sont envoyés en texte clair, juste codés en base64, ce qui peut être facilement décodé s'ils sont interceptés.

Pour une sécurité améliorée, des alternatives comme OAuth 2.0 sont recommandées, surtout lorsqu'il s'agit de données ou d'opérations sensibles. Google Apps Script dispose d'un support intégré pour OAuth 2.0 avec la bibliothèque `OAuth2`, simplifiant le processus d'authentification contre les services prenant en charge ce protocole.

Malgré ses limitations de sécurité, l'authentification de base reste largement utilisée pour des applications simples ou internes non exposées à l'internet plus large. Elle est simple à mettre en œuvre, car elle ne nécessite qu'une seule requête avec des en-têtes correctement définis, ce qui en fait une option attrayante pour des intégrations rapides ou pour des API où des méthodes de sécurité plus élevées ne sont pas disponibles. Cependant, les programmeurs sont encouragés à considérer les implications de sécurité et à explorer des alternatives plus sûres lorsque cela est possible.
