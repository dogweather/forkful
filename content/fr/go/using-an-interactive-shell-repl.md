---
title:                "Utilisation d'une console interactive (REPL)"
date:                  2024-01-26T04:14:44.052684-07:00
model:                 gpt-4-0125-preview
simple_title:         "Utilisation d'une console interactive (REPL)"
programming_language: "Go"
category:             "Go"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/go/using-an-interactive-shell-repl.md"
---

{{< edit_this_page >}}

## Quoi et pourquoi ?
Un REPL (Boucle Lire-Évaluer-Afficher) vous permet de interagir en direct avec le code ; il lit l'entrée, l'évalue, affiche le résultat, et recommence. Les programmeurs l'utilisent pour tester des extraits de code, déboguer et apprendre de nouveaux langages en temps réel.

## Comment faire :
Go n'inclut pas de REPL intégré, mais vous pouvez utiliser des outils tiers. Un outil populaire est `gore` :

```go
// Installer gore avec
$ go install github.com/motemen/gore/cmd/gore@latest

// Exécuter gore
$ gore
gore version 0.5.0  :help pour de l'aide
gore> :import fmt
gore> fmt.Println("Bonjour, Go REPL !")
Bonjour, Go REPL !
nil
```

## Exploration approfondie
Initialement développés pour Lisp, les REPL sont courants dans les langages dynamiques comme Python ou Ruby. Go, étant typé statiquement, n'en inclut pas directement. Des alternatives à `gore` comprennent `go-pry` et `yaegi`. Ces outils interprètent le code Go, vous permettant d'explorer et de valider des idées rapidement sans compiler une application complète. Ils sont particulièrement utiles pour les débutants et dans des contextes éducatifs où l'accent est mis sur l'apprentissage et l'expérimentation.

## Voir également
- `gore` : [https://github.com/motemen/gore](https://github.com/motemen/gore)
- `go-pry` : [https://github.com/d4l3k/go-pry](https://github.com/d4l3k/go-pry)
- `yaegi` : [https://github.com/traefik/yaegi](https://github.com/traefik/yaegi)