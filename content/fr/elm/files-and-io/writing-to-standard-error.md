---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:33:00.326327-07:00
description: "\xC9crire dans l'erreur standard (stderr) consiste \xE0 rediriger les\
  \ messages d'erreur et les diagnostics s\xE9par\xE9ment du r\xE9sultat principal\
  \ du programme, qui va\u2026"
lastmod: '2024-03-13T22:44:57.707261-06:00'
model: gpt-4-0125-preview
summary: "\xC9crire dans l'erreur standard (stderr) consiste \xE0 rediriger les messages\
  \ d'erreur et les diagnostics s\xE9par\xE9ment du r\xE9sultat principal du programme,\
  \ qui va \xE0 la sortie standard (stdout)."
title: "\xC9crire sur l'erreur standard"
weight: 25
---

## Quoi & Pourquoi ?

Écrire dans l'erreur standard (stderr) consiste à rediriger les messages d'erreur et les diagnostics séparément du résultat principal du programme, qui va à la sortie standard (stdout). Les programmeurs le font pour rendre la gestion des erreurs et la journalisation plus gérables, surtout dans les environnements où la distinction des sorties est cruciale pour le débogage et la surveillance.

## Comment faire :

Elm est principalement destiné au développement Web, où le concept d'écriture directe sur stderr ne s'applique pas de la même manière que dans les environnements en ligne de commande traditionnels. Cependant, pour les programmes Elm s'exécutant dans Node.js ou des environnements similaires, l'interopérabilité avec JavaScript en utilisant les ports est l'approche clé pour atteindre une fonctionnalité similaire. Voici comment vous pourriez le configurer :

Code Elm (`Main.elm`) :
```elm
port module Main exposing (main)

import Browser

port errorOut : String -> Cmd msg

-- Exemple de fonction fictive qui envoie un message d'erreur à JS
generateError : String -> Cmd msg
generateError message =
    errorOut message

main =
    generateError "Ceci est un message d'erreur pour stderr"
```

Interopérabilité JavaScript (`index.js`) :
```javascript
const { Elm } = require('./Main.elm');

var app = Elm.Main.init();

app.ports.errorOut.subscribe((message) => {
  console.error(message);
});
```
Ce code Elm définit un port `errorOut` qui permet d'envoyer des messages hors d'Elm vers JavaScript. Puis, dans le code JavaScript, nous écoutons les messages envoyés à travers ce port et les redirigeons vers stderr en utilisant `console.error()`. De cette façon, vous pouvez effectivement écrire dans stderr dans un environnement qui le prend en charge, en tirant parti des fonctionnalités d'interopérabilité d'Elm avec JavaScript.

Sortie exemple dans le terminal Node.js (lorsque `index.js` est exécuté) :
```
Ceci est un message d'erreur pour stderr
```
