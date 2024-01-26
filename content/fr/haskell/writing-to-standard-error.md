---
title:                "Écrire dans l'erreur standard"
html_title:           "Arduino: Écrire dans l'erreur standard"
simple_title:         "Écrire dans l'erreur standard"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/haskell/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?
Écrire sur la sortie d'erreur standard (stderr) permet d'envoyer des messages d'erreur sans les mélanger avec la sortie normale (stdout). Les programmeurs utilisent stderr pour diagnostiquer les problèmes sans perturber le flux de données principal.

## Comment faire :
```Haskell
import System.IO

main = do
  hPutStrLn stderr "Ceci est un message d'erreur."
```
Sortie attendue (à afficher dans la console d'erreur) :
```
Ceci est un message d'erreur.
```

## Plongée profonde
Historiquement, les flux stdout et stderr sont des concepts hérités des systèmes Unix, permettant la séparation des données de sortie normales et des messages d'erreur. D'autres alternatives incluent l'écriture dans des fichiers de log, mais stderr reste le canal direct et standardisé pour rapporter les erreurs. En Haskell, `System.IO` gère ces flux, offrant des fonctions telles que `hPutStrLn` pour interagir spécifiquement avec stderr.

## Voir également
- Documentation Haskell pour `System.IO`: https://hackage.haskell.org/package/base-4.16.1.0/docs/System-IO.html
- Guide sur l'utilisation des flux de données Unix: https://tldp.org/HOWTO/Bash-Prog-Intro-HOWTO-3.html
- Explication détaillée des flux stdout et stderr: https://www.jstor.org/stable/2582047
