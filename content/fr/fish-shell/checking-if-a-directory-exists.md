---
title:                "Vérification de l'existence d'un répertoire"
html_title:           "Bash: Vérification de l'existence d'un répertoire"
simple_title:         "Vérification de l'existence d'un répertoire"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/fish-shell/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## What & Why? (Quoi & Pourquoi ?)
Déterminer si un répertoire existe permet d'éviter les erreurs lors de l'accès à des fichiers. Les programmeurs le font pour s'assurer que leurs scripts se comportent correctement.

## How to (Comment faire) :
Voici comment vérifier l'existence d'un répertoire dans Fish Shell :

```Fish Shell
if test -d /chemin/vers/repertoire
    echo "Le répertoire existe."
else
    echo "Le répertoire n'existe pas."
end
```

Assurez-vous de remplacer `/chemin/vers/repertoire` par le chemin réel vers votre répertoire.

## Deep Dive (Plongée en profondeur) :
Historiquement, la commande `test` (aussi représentée par `[` ou `[[` en bash) est utilisée pour évaluer des expressions conditionnelles. Dans Fish Shell, la syntaxe est principalement la même, ce qui facilite la transition pour les nouveaux utilisateurs. En alternative, vous pouvez utiliser `if not test -d /chemin/vers/repertoire`, qui inverse la logique, ou des commandes comme `stat -c %d` pour obtenir des informations plus détaillées sur le dossier. La spécificité de Fish réside dans sa syntaxe simplifiée et sa lisibilité pour l'exécution de telles vérifications.

## See Also (Voir aussi) :
- [Documentation officielle](https://fishshell.com/docs/current/index.html)
- [Tutoriels Fish Shell](https://fishshell.com/docs/current/tutorial.html)
- [Guide des meilleures pratiques](https://github.com/jorgebucaran/fisher/blob/master/README.md)