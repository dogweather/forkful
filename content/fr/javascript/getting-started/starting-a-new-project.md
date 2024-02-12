---
title:                "Lancement d'un nouveau projet"
aliases:
- /fr/javascript/starting-a-new-project.md
date:                  2024-01-20T18:03:56.352677-07:00
model:                 gpt-4-1106-preview
simple_title:         "Lancement d'un nouveau projet"

tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/javascript/starting-a-new-project.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?
Démarrer un nouveau projet, c'est comme mettre une graine dans le sol numérique. Les programmeurs le font pour créer du neuf, résoudre des problèmes, ou simplement pour le plaisir de coder.

## Comment faire :
Démarrer un nouveau projet JavaScript, c'est souvent initialiser un environnement avec `npm` et configurer le tout avec un `package.json`.

```Javascript
// Initialisation d'un nouveau projet
npm init -y // Crée un package.json avec les valeurs par défaut

// Installation d'un paquet pour exemple
npm install express // Installe express, un framework web
```

Après l'installation, votre `package.json` ressemblera à quelque chose comme ça :

```JSON
{
  "name": "mon-projet",
  "version": "1.0.0",
  "dependencies": {
    "express": "^4.17.1"
  }
  // autres métadonnées...
}
```

Pour démarrer un script simple :

```Javascript
const express = require('express');
const app = express();

app.get('/', (req, res) => res.send('Bonjour le monde!'));

app.listen(3000, () => console.log('Serveur démarré sur le port 3000'));
```

Sortie attendue dans le terminal :

```
Serveur démarré sur le port 3000
```

## Deep Dive
Au début, JavaScript n'était utilisé que côté client. Node.js a changé la donne en 2009, rendant JavaScript disponible côté serveur. Aujourd'hui, avec des gestionnaires de paquets comme `npm` ou `yarn`, démarrer un projet est devenu une affaire de quelques lignes de commande. Il existe des alternatives comme `deno` qui vise à simplifier encore plus les choses.

Pour plus de détails, la documentation de Node.js ou les guides de gestionnaires de paquet sont de bonnes ressources pour maîtriser la création de projets.

## Voir aussi
- [Documentation npm](https://docs.npmjs.com/)
- [Express.js](https://expressjs.com/fr/)
- [Node.js Guides](https://nodejs.org/en/docs/guides/)
- [Deno land](https://deno.land/)
