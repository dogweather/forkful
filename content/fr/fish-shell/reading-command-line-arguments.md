---
title:                "Lecture des arguments de ligne de commande"
html_title:           "Ruby: Lecture des arguments de ligne de commande"
simple_title:         "Lecture des arguments de ligne de commande"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/fish-shell/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Qu'est-ce & Pourquoi?
Lecture d'arguments de ligne de commande signifie que notre programme récupère des données directement saisies par l'utilisateur lors de l'exécution. Les programmeurs le font pour rendre le programme plus interactif, et peuvent modifier le comportement du programme sans modifier le code source.

## Comment faire:
La façon la plus simple d'accéder aux arguments de ligne de commande en Fish Shell est d'utiliser la variable préétablie `argv`. Voyons comment ça fonctionne.
```Fish Shell
function afficher_args
    for arg in $argv
        echo "L'argument est: $arg"
    end
end
```
Appelons notre fonction avec quelques arguments:
```Fish Shell
afficher_args ceci est un test
```
Cela produit la sortie:
```Fish Shell
L'argument est: ceci
L'argument est: est
L'argument est: un
L'argument est: test
```
## Plongée en profondeur
Historiquement, les arguments de ligne de commande sont une fonctionnalité présente dans les premiers systèmes Unix, offrant une manière simple d'interagir directement avec des scripts et des programmes. Dans Fish Shell, nous avons `argv` comme alternative. C'est une variable globale, accessible partout dans votre script.

Se familiariser avec les arguments de ligne de commande en Fish Shell est crucial, surtout si vous envisagez de créer des scripts interactifs et personnalisables. 

## Voir aussi
Pour plus d'informations sur la programmation en Fish Shell:
- La page de manuel officielle de Fish Shell: [Link](https://fishshell.com/docs/current/index.html)
- Un tutoriel complet sur la lecture des arguments de ligne de commande en Fish Shell :
[Link](https://fishshell.com/docs/current/tutorial.html#tut_scripting)