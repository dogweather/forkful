---
date: 2024-01-20 17:44:47.888949-07:00
description: "T\xE9l\xE9charger une page web, c'est r\xE9cup\xE9rer ses donn\xE9es\
  \ (HTML, CSS, scripts) pour les manipuler ou les stocker. Les programmeurs le font\
  \ pour analyser le\u2026"
lastmod: '2024-03-11T00:14:31.451760-06:00'
model: gpt-4-1106-preview
summary: "T\xE9l\xE9charger une page web, c'est r\xE9cup\xE9rer ses donn\xE9es (HTML,\
  \ CSS, scripts) pour les manipuler ou les stocker. Les programmeurs le font pour\
  \ analyser le\u2026"
title: "T\xE9l\xE9chargement d'une page web"
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?

Télécharger une page web, c'est récupérer ses données (HTML, CSS, scripts) pour les manipuler ou les stocker. Les programmeurs le font pour analyser le contenu, surveiller les changements, ou alimenter des bases de données.

## Comment faire :

```TypeScript
import axios from 'axios'; // N'oubliez pas d'installer axios avec npm ou yarn

async function telechargerPageWeb(url: string): Promise<void> {
    try {
        const response = await axios.get(url);
        console.log(response.data); // Voici le contenu de la page web
    } catch (error) {
        console.error(`Erreur lors du téléchargement de la page : ${error}`);
    }
}

// Utilisez la fonction
telechargerPageWeb('https://www.exemple.com');
```

Exemple de sortie :

```
<!DOCTYPE html>
<html lang="fr">
<head>
    <meta charset="UTF-8">
    <title>Exemple de Page Web</title>
</head>
<body>
    <p>Ceci est un exemple de contenu d'une page web...</p>
</body>
</html>
```

## Plongée profonde

Historiquement, les pages web étaient téléchargées en utilisant XMLHttpRequest, mais ce standard a été remplacé par l'API Fetch plus moderne, qui est basée sur les promesses. TypeScript, étant un sur-ensemble de JavaScript, permet d'utiliser ces API et d'autres outils comme `axios`, qui simplifie les requêtes HTTP.

Il existe d'autres alternatives, comme `node-fetch` ou les librairies de bas niveau comme `http` et `https` modules de Node.js, qui donnent un contrôle plus fin.

Concernant l'implémentation, prenez en compte la gestion des erreurs, le réglage des en-têtes HTTP pour gérer la politique de même origine (CORS), et l'encodage correct des caractères de la page.

## Voir aussi

- Documentation Axios : [https://axios-http.com/docs/intro](https://axios-http.com/docs/intro)
- API Fetch pour les navigateurs : [https://developer.mozilla.org/en-US/docs/Web/API/Fetch_API](https://developer.mozilla.org/en-US/docs/Web/API/Fetch_API)
- Module HTTP de Node.js : [https://nodejs.org/api/http.html](https://nodejs.org/api/http.html)
