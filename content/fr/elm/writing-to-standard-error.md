---
title:                "Écrire dans l'erreur standard"
date:                  2024-01-19
simple_title:         "Écrire dans l'erreur standard"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/elm/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## What & Why?
En programmation, écrire sur l'erreur standard (stderr) permet de séparer les messages d'erreurs du flux de sortie normal. On le fait pour diagnostiquer les problèmes sans perturber la sortie attendue du programme.

## How to:
Elm tourne dans le navigateur et n'a pas d'accès direct à stderr comme les langages côté serveur. Mais, on peut simuler un comportement semblable en utilisant `Console.error` avec JavaScript interop via ports.

```Elm
port module Main exposing (..)

-- Port pour envoyer des messages d'erreur à JavaScript
port error : String -> Cmd msg

-- Utiliser le port dans votre application Elm
reportError : String -> Cmd msg
reportError message =
    error message

-- Exemple d'utilisation
main =
    reportError "Une erreur est survenue"
```

Sur le côté JavaScript, vous devrez souscrire au port et l’utiliser pour écrire sur stderr.

```javascript
app.ports.error.subscribe(function (message) {
  console.error(message);
});
```

## Deep Dive
Historiquement, Elm est conçu pour la sécurité et la facilité, sans accès direct à stderr ou stdout vu qu'il est exécuté dans un contexte de navigateur. Si vous avez besoin d'une gestion des erreurs côté serveur avec stdout et stderr, envisagez de combiner Elm avec Node.js. Alternativement, utilisez des outils comme `elm-debug-transformer`, qui enrichit les messages de débogage.

## See Also
- Elm Ports Documentation: [https://guide.elm-lang.org/interop/ports.html](https://guide.elm-lang.org/interop/ports.html)
- `elm-debug-transformer` pour une expérience améliorée du débogage dans le navigateur: [https://github.com/kraklin/elm-debug-transformer](https://github.com/kraklin/elm-debug-transformer)
