---
title:                "Lecture des arguments de ligne de commande"
date:                  2024-01-20T17:56:44.882574-07:00
model:                 gpt-4-1106-preview
simple_title:         "Lecture des arguments de ligne de commande"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/swift/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## What & Why? (Quoi et Pourquoi ?)
Lire les arguments de la ligne de commande, c'est récupérer des valeurs fournies lors de l'exécution d'un programme en console. Les programmeurs utilisent ces arguments pour configurer ou influencer le comportement du programme sans changer le code.

## How to (Comment faire) :
Swift rend la lecture des arguments de la ligne de commande presque trop facile. Voici comment on fait :

```Swift
// main.swift

// Affichez tous les arguments de la ligne de commande
for argument in CommandLine.arguments {
    print(argument)
}

// Utilisez les arguments (supposez que le premier est toujours le nom du programme)
if CommandLine.argc > 1 {
    let firstArgument = CommandLine.arguments[1]
    print("Premier argument est: \(firstArgument)")
}
```

Si vous exécutez ce programme comme ça : `swift main.swift Salut Toi`,
il va afficher :

```
/path/to/main.swift
Salut
Toi
Premier argument est: Salut
```

## Deep Dive (Plongée en profondeur) :
Historiquement, accéder aux arguments de la ligne de commande est un concept ancien, présent dès les premiers systèmes d'exploitation en mode texte. Swift, modernise l'accès à ces informations avec l'objet `CommandLine`, remplaçant l'ancien style `argc` et `argv` d'autres langages comme C.

Les alternatives pour Swift incluent l'utilisation de bibliothèques tiers comme 'Swift Argument Parser' pour les applications plus complexes. En plus de récupérer les arguments, il est souvent utile de les analyser ; pour cela, des options comme `getopt()` ou des bibliothèques de parsing peuvent être envisagées.

Côté mise en œuvre, `CommandLine` est un énumération statique en Swift qui offre un accès direct aux arguments. Une différence notable avec d'autres langages est que `CommandLine.arguments` inclut le chemin d'accès du programme comme son premier élément, alors qu'en C, `argv[0]` est le nom du programme.

## See Also (Voir Aussi) :
- Documentation Swift sur `CommandLine`: https://developer.apple.com/documentation/swift/commandline
- Swift Argument Parser, une librairie pour créer des outils de ligne de commande : https://github.com/apple/swift-argument-parser
- Article sur le parsing des arguments de ligne de commande en Swift : https://www.swiftbysundell.com/articles/command-line-parsing-in-swift/
