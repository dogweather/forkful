---
title:                "Lecture des arguments de ligne de commande"
html_title:           "Swift: Lecture des arguments de ligne de commande"
simple_title:         "Lecture des arguments de ligne de commande"
programming_language: "Swift"
category:             "Swift"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/swift/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Pourquoi

Si vous êtes un programmeur en herbe ou expérimenté, vous avez probablement entendu parler des arguments de ligne de commande dans vos projets de programmation. Mais pourquoi devriez-vous vous intéresser à cette fonctionnalité ? Eh bien, voyons ensemble pourquoi vous devriez apprendre à lire les arguments de ligne de commande en Swift.

## Comment faire

Commençons par comprendre ce que sont les arguments de ligne de commande. Ce sont des valeurs spécifiées lors de l'exécution d'un programme, qui peuvent être utilisées pour personnaliser ou configurer le fonctionnement du programme. En Swift, pour lire ces arguments, vous pouvez utiliser l'objet `CommandLine`.

Voici un exemple de code qui illustre comment lire et afficher les arguments de ligne de commande en utilisant l'objet `CommandLine` :

```Swift
//Premier argument : nom du programme
print(CommandLine.arguments[0])

//Deuxième argument : premier argument spécifié par l'utilisateur
let arg1 = CommandLine.arguments[1]
print("Premier argument : \(arg1)")

//Troisième argument : deuxième argument spécifié par l'utilisateur
let arg2 = CommandLine.arguments[2]
print("Deuxième argument : \(arg2)")
```

Ainsi, si vous exécutez votre programme avec les arguments `Bonjour` et `le monde`, la sortie sera la suivante :

```
nomduprogramme
Premier argument : Bonjour
Deuxième argument : le monde
```

## Plongée en profondeur

Maintenant que vous savez comment lire les arguments de ligne de commande en Swift, allons un peu plus loin. Vous pouvez également ajouter des options à vos arguments de ligne de commande, afin de donner plus de flexibilité à votre programme. Les options sont précédées d'un tiret (`-`) et peuvent être suivies d'un paramètre.

Voici un exemple de code qui utilise des options pour afficher un message de bienvenue personnalisé :

```Swift
//Définition des options
let nomOption = CommandLine.Option("n", "--nom", description: "Définit le nom de l'utilisateur")
let ageOption = CommandLine.Option("a", "--age", description: "Définit l'âge de l'utilisateur")

//Parsing des options
let options = CommandLine.parseOptions()

//Récupération des valeurs des options spécifiées par l'utilisateur
let nom = options[nomOption]
let age = options[ageOption]

//Affichage du message de bienvenue
print("Bienvenue \(nom ?? "inconnu"), vous avez \(age ?? "inconnu") ans !")
```

Ainsi, si vous exécutez votre programme avec les options `-n Bob` et `-a 25`, la sortie sera la suivante :

```
Bienvenue Bob, vous avez 25 ans !
```

Il existe de nombreuses autres fonctions utiles pour lire et manipuler les arguments de ligne de commande en Swift, telles que `CommandLine.popFirst()` pour récupérer le premier argument et le supprimer de la liste, ou encore `CommandLine.arguments.contains()` pour vérifier si un argument spécifique a été utilisé.

## Voir aussi

Vous pouvez en apprendre davantage sur les arguments de ligne de commande en Swift en consultant les liens suivants :

- [Documentation officielle Apple](https://developer.apple.com/documentation/foundation/commandline)
- [Tutoriel raywenderlich.com](https://www.raywenderlich.com/511-command-line-programs-on-macos-tutorial)
- [Cours Udacity](https://www.udacity.com/course/command-line-programs-in-swift--ud207)