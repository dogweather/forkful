---
title:                "Téléchargement d'une page web"
date:                  2024-01-20T17:44:08.743101-07:00
model:                 gpt-4-1106-preview
simple_title:         "Téléchargement d'une page web"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/javascript/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## What & Why?
Télécharger une page web, c'est récupérer son contenu via le Net. Les programmeurs le font pour analyser des données, tester la disponibilité ou collecter des infos automatiquement.

## How to:

Pour télécharger une page web en JavaScript, on utilise souvent la librairie `axios` ou la fonction `fetch`. Voici un exemple avec `fetch`:

```javascript
fetch('https://example.com')
  .then(response => response.text())
  .then(data => {
    console.log(data); // affiche le contenu HTML de la page
  })
  .catch(error => {
    console.error('Erreur lors du téléchargement:', error);
  });
```

Sortie d’exemple :

```
<!doctype html>
<html>
<head>
    <title>Exemple de titre</title>
...
```

## Deep Dive:

Historiquement, on utilisait `XMLHttpRequest` pour faire ce job. Mais c'était un peu lourd et moins intuitif. Avec l’arrivée de `fetch` dans les normes modernes, les choses se sont simplifiées. `Fetch` est basé sur les promesses, donc il est plus flexible et mieux adapté pour des opérations asynchrones.

Il y a aussi `axios`, une librairie tierce qui offre plus de fonctionnalités comme l'annulation de requêtes, mais elle doit être incluse séparément.

Pour les détails d'implémentation, `fetch` envoie une requête HTTP et renvoie une promesse qui, une fois résolue, donne accès au corps de la réponse. Il est important de vérifier le statut de la réponse pour gérer les différentes situations (succès, erreur, redirection, etc.).

## See Also:

- MDN Web Docs sur `fetch`: https://developer.mozilla.org/fr/docs/Web/API/Fetch_API/Using_Fetch
- Documentation de `axios`: https://axios-http.com/docs/intro
- Comparaison entre `fetch` et `XMLHttpRequest`: https://developer.mozilla.org/fr/docs/Web/API/Fetch_API/Basic_concepts_around_fetch