---
title:    "Swift: Lecture des arguments de ligne de commande"
keywords: ["Swift"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/swift/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Pourquoi 

La lecture des arguments de ligne de commande est une compétence essentielle pour les développeurs Swift. Que vous soyez un débutant qui apprend les bases du langage ou un développeur expérimenté cherchant à améliorer ses compétences, la compréhension de cette fonctionnalité vous permettra de créer des applications plus dynamiques et efficaces. Dans cet article, nous allons vous expliquer pourquoi la lecture des arguments de ligne de commande est importante et comment le faire en Swift.

## Comment faire 

La lecture des arguments de ligne de commande consiste à récupérer les valeurs saisies par l'utilisateur lorsqu'il lance l'application depuis le terminal. Pour cela, nous allons utiliser l'objet `ProcessInfo` qui contient toutes les informations sur le processus en cours d'exécution, y compris les arguments de ligne de commande. Jetons un coup d'œil à un exemple de code:

```Swift
// Récupération des arguments de ligne de commande
let arguments = ProcessInfo.processInfo.arguments

// Parcours des arguments
for argument in arguments[1...] {
    print(argument)
}

```
Dans cet exemple, nous utilisons la propriété `arguments` de `ProcessInfo` pour stocker tous les arguments de la ligne de commande dans un tableau. Nous utilisons ensuite une boucle pour parcourir ces arguments et les afficher à l'écran.

Si nous ouvrons notre terminal et lançons l'application avec des arguments, par exemple "monApplication bonjour", nous obtiendrons la sortie suivante:

```bash
bonjour
```
Comme vous pouvez le constater, nous avons réussi à récupérer l'argument "bonjour" et à le stocker dans notre tableau. Vous pouvez également utiliser cette méthode pour stocker les arguments dans des variables et les utiliser dans votre code selon vos besoins.

## Deep Dive 

Pour ceux d'entre vous qui veulent en savoir plus, voici quelques éléments supplémentaires sur la lecture des arguments de ligne de commande en Swift. Tout d'abord, gardez à l'esprit que le premier argument stocké dans la propriété `arguments` de `ProcessInfo` est toujours le nom de l'application elle-même. Cela signifie que si vous souhaitez parcourir tous les arguments saisis par l'utilisateur, vous devez commencer par l'élément à l'index 1 de votre tableau.

De plus, vous pouvez également utiliser la méthode `arguments.joined(separator:)` pour obtenir tous les arguments sous forme de chaîne de caractères, en spécifiant un séparateur de votre choix. Par exemple, si vous souhaitez afficher tous les arguments séparés par un espace, vous pouvez utiliser cette méthode de la manière suivante:

```Swift
let arguments = ProcessInfo.processInfo.arguments.joined(separator: " ")
print(arguments)
```

Enfin, il est important de noter que la lecture des arguments de ligne de commande en Swift n'est possible que pour les applications en ligne de commande, car les applications graphiques n'ont pas accès à ces informations.

## Voir aussi 

Pour en savoir plus sur la lecture des arguments de ligne de commande en Swift, vous pouvez consulter les liens suivants:

- [Documentation Apple sur `ProcessInfo`](https://developer.apple.com/documentation/foundation/processinfo)
- [Article sur la manipulation des arguments de ligne de commande en Swift](https://www.swiftbysundell.com/articles/managing-command-line-arguments-in-swift/)

Maintenant que vous comprenez l'importance et la manière de lire les arguments de ligne de commande en Swift, n'hésitez pas à l'appliquer dans vos propres projets et à explorer d'autres fonctionnalités que Swift a à offrir. À bientôt pour un nouvel article sur la programmation Swift !