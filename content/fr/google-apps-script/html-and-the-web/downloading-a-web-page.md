---
title:                "Télécharger une page web"
aliases:
- /fr/google-apps-script/downloading-a-web-page.md
date:                  2024-02-01T21:52:34.001905-07:00
model:                 gpt-4-0125-preview
simple_title:         "Télécharger une page web"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/google-apps-script/downloading-a-web-page.md"
changelog:
  - 2024-02-01, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?

Télécharger une page web avec Google Apps Script implique de récupérer le contenu d'une page web via HTML pour diverses raisons, telles que le web scraping, l'extraction de données ou la surveillance des changements. Les programmeurs optent pour cette opération pour automatiser la collecte ou les tâches d'intégration de données, minimisant l'effort manuel et assurant un traitement des données en temps réel.

## Comment faire :

Dans Google Apps Script, le service `UrlFetchApp` est essentiel pour télécharger le contenu web. Voici un guide étape par étape et un exemple simple montrant comment récupérer et consigner le contenu HTML d'une page web :

1. **Opération de récupération de base :**

```javascript
function downloadWebPage() {
  var url = "http://example.com";
  var response = UrlFetchApp.fetch(url);
  var content = response.getContentText();
  Logger.log(content);
}
```

- Ce code récupère le contenu HTML de example.com et le consigne. C'est une démonstration simple de l'obtention du code source d'une page web sans paramètres supplémentaires.

2. **Gestion des redirections et HTTPS :**

Pour les HTTPS ou la gestion des redirections, le code reste largement le même, mais envisagez de mettre en œuvre une gestion des erreurs ou des options spécifiques pour les redirections :

```javascript
function downloadSecureWebPage() {
  var options = {
    'followRedirects': true, // Suivre automatiquement les redirections
    'muteHttpExceptions': true // Rendre muettes les éventuelles exceptions pour les gérer avec grâce
  };
  
  var url = "https://example.com";
  var response = UrlFetchApp.fetch(url, options);
  Logger.log(response.getContentText());
}
```

3. **Limites de fréquence et quotas :**

Soyez attentif aux quotas de Google Apps Script ; une utilisation intensive peut nécessiter une gestion des erreurs pour les limites de fréquence.

## Plongée Profonde

Historiquement, le téléchargement et la manipulation de contenu web ont commencé avec de simples requêtes HTTP, évoluant de manière significative avec l’avènement des langages de script. Google Apps Script permet l'exécution simple de telles tâches au sein de l'écosystème G Suite, en tirant parti de l'infrastructure robuste de Google. Le service `UrlFetchApp` est un élément central de cette fonctionnalité, encapsulant des requêtes HTTP/S complexes dans une interface plus simple au niveau de l'application.

Malgré sa commodité, Google Apps Script n'est pas toujours l'outil le plus adapté pour le web scraping intensif ou lorsque un post-traitement complexe des données récupérées est nécessaire en raison des limites de temps d'exécution et des quotas imposés par Google. Dans de tels cas, des cadres de web scraping dédiés ou des langages conçus pour les opérations E/S asynchrones, tels que Node.js avec des bibliothèques comme Puppeteer ou Cheerio, pourraient offrir plus de flexibilité et de puissance.

De plus, bien que Google Apps Script soit un excellent outil pour l'intégration aux services Google (comme Sheets, Docs et Drive) et pour effectuer des opérations de récupération de données légères, il est crucial de garder à l'esprit les limitations de son environnement d'exécution. Pour des tâches intensives, envisagez d'utiliser Google Cloud Functions ou les services avancés d'Apps Script avec des ressources de calcul externes pour le traitement.
