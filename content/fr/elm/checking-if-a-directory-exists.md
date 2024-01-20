---
title:                "Vérification de l'existence d'un répertoire"
html_title:           "Elm: Vérification de l'existence d'un répertoire"
simple_title:         "Vérification de l'existence d'un répertoire"
programming_language: "Elm"
category:             "Elm"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/elm/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?

Vérifier l'existence d'un répertoire est une pratique courante qui permet de savoir si un dossier particulier est présent dans le système de fichiers ou non. Les programmeurs le font souvent pour éviter les erreurs lors de la lecture, de l'écriture ou de la modification de fichiers dans ce répertoire.

## Comment faire :

Elm est un langage qui s'exécute dans le navigateur et n'a pas directement accès au système de fichiers. Cependant, il peut communiquer avec le JavaScript, qui peut interagir avec un serveur ou utiliser certaines API de navigateur pour accéder au système de fichiers local dans certaines conditions.

```Elm
port module Main exposing (..)

-- A 'port' declaration creates simple 
-- channels for sending data to and receiving data from JavaScript.

port dirExists : (String -> msg) -> Sub msg

main =
    Browser.sandbox
        { init = init
        , view = view
        , update = update
        }
```
Dans JavaScript, vous pourriez avoir :
```JavaScript
var app = Elm.Main.init();

app.ports.dirExists.subscribe(function(directory) {
    try{
        var stat = fs.lstatSync(directory);
        app.ports.dirExists.send(stat.isDirectory());
    }catch(e){
        app.ports.dirExists.send(false);
    }
});
```

## Un Aperçu en Profondeur :

Historiquement, Elm n'a pas été conçu pour interagir directement avec le système de fichiers pour des raisons de sécurité et d'abstraction. Cependant, en utilisant 'ports', Elm peut envoyer et recevoir des informations depuis JavaScript, qui peut accéder au système de fichiers.

Des alternatives à cette approche existent, principalement par l'utilisation d'autres langages qui ont accès direct au système de fichiers, comme Node.js, Python ou Bash. Cependant, si vous voulez utiliser Elm, utiliser 'ports' est une solution courante.

L'utilisation de `lstatSync` de Node.js returne les informations sur le fichier ou répertoire. Si le répertoire n'existe pas, une erreur sera générée et capturée par le bloc `catch`.

## Voir Aussi :

Pour plus d'informations sur ce sujet, vous pouvez consulter les ressources suivantes :

- Documentation Elm sur les 'ports' : https://guide.elm-lang.org/interop/ports.html
- Documentation Node.js sur le système de fichiers : https://nodejs.org/api/fs.html
- Une introduction à Elm : https://guide.elm-lang.org/