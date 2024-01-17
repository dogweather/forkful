---
title:                "Lecture des arguments de la ligne de commande"
html_title:           "Go: Lecture des arguments de la ligne de commande"
simple_title:         "Lecture des arguments de la ligne de commande"
programming_language: "Go"
category:             "Go"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/go/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

# Qu'est-ce que lire les arguments de la ligne de commande et pourquoi est-ce important pour les programmeurs ?

Lire les arguments de la ligne de commande consiste à récupérer les informations saisies par l'utilisateur au moment d'exécuter un programme en ligne de commande. Les programmeurs font cela pour pouvoir interagir avec l'utilisateur et lui permettre de spécifier des options ou des paramètres personnalisés pour l'exécution du programme.

# Comment faire :

```Go
// Déclaration d'un slice pour stocker les arguments de la ligne de commande
args := os.Args[1:]

// Parcours et affichage des arguments
for i, arg := range args {
	fmt.Printf("Argument %d : %s\n", i+1, arg)
}

// Exemple d'exécution : go run main.go arg1 arg2
// Résultat : Argument 1 : arg1
//            Argument 2 : arg2
```

# Approfondissement :

## Contexte historique :

La lecture des arguments de la ligne de commande est un concept qui existe depuis les débuts de la programmation informatique. Avec l'émergence des interfaces graphiques, elle a un peu perdu de son importance mais reste un outil essentiel pour les programmes fonctionnant en ligne de commande.

## Alternatives :

Une alternative à la lecture des arguments de la ligne de commande peut être l'utilisation de fichiers de configuration stockant les options et paramètres du programme. Cependant, cela nécessite un traitement supplémentaire pour lire et interpréter ces fichiers, tandis que les arguments de la ligne de commande sont directement accessibles dans le code.

## Détails d'implémentation :

En Go, les arguments de la ligne de commande sont stockés dans un slice du type []string, accessible via la variable os.Args. Le premier élément de ce slice est toujours le nom du programme lui-même. L'utilisation de la fonction range dans une boucle for permet de parcourir tous les arguments de manière efficace.

# Voir aussi :

- Documentation officielle sur la lecture des arguments de la ligne de commande en Go : https://golang.org/pkg/os/#pkg-variables
- Un guide approfondi sur l'utilisation des arguments de la ligne de commande en Go : https://gobyexample.com/command-line-arguments