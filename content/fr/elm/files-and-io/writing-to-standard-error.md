---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:33:00.326327-07:00
description: "Comment faire : Elm est principalement destin\xE9 au d\xE9veloppement\
  \ Web, o\xF9 le concept d'\xE9criture directe sur stderr ne s'applique pas de la\
  \ m\xEAme mani\xE8re que\u2026"
lastmod: '2024-03-13T22:44:57.707261-06:00'
model: gpt-4-0125-preview
summary: "Elm est principalement destin\xE9 au d\xE9veloppement Web, o\xF9 le concept\
  \ d'\xE9criture directe sur stderr ne s'applique pas de la m\xEAme mani\xE8re que\
  \ dans les environnements en ligne de commande traditionnels."
title: "\xC9crire sur l'erreur standard"
weight: 25
---

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
