---
title:    "Go: La lecture des arguments en ligne de commande"
keywords: ["Go"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/go/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Pourquoi

Lorsque vous créez des programmes en Go, il est important de pouvoir les exécuter avec différentes options et paramètres. Cela peut être utile lors du développement ou pour une utilisation en production. Dans cet article, nous allons vous montrer comment lire les arguments de ligne de commande en utilisant Go.

## Comment faire

Pour lire les arguments de ligne de commande en Go, nous allons utiliser la fonction `os.Args`. Cette fonction nous permet de récupérer une liste de toutes les valeurs saisies après le nom du programme lors de son exécution.

Voici un exemple de code pour lire les arguments de ligne de commande et les afficher :

```Go
for index, arg := range os.Args {
	fmt.Printf("Argument %d : %s\n", index, arg)
}
```

Lorsque vous exécutez ce code en saisissant `go run main.go hello world` dans votre terminal, vous obtiendrez la sortie suivante :

```
Argument 0 : main.go
Argument 1 : hello
Argument 2 : world
```

Comme vous pouvez le voir, la liste d'arguments contient le nom du programme en premier, suivi de chaque argument séparé par un espace. Vous pouvez ensuite utiliser ces valeurs pour effectuer différentes actions en fonction de vos besoins.

## Plongée en profondeur

En plus de la fonction `os.Args`, Go offre également d'autres options pour lire les arguments de ligne de commande de manière plus détaillée. Par exemple, vous pouvez utiliser le package `flag` pour définir des options spécifiques et leurs valeurs associées.

Voici un exemple de code pour définir une option `-name` avec une valeur par défaut et utiliser cette option pour afficher un message personnalisé :

```Go
name := flag.String("name", "World", "Specify a name for a personalized message.")
flag.Parse()

fmt.Printf("Hello %s!", *name)
```

En exécutant ce code en ajoutant `-name John` en tant qu'argument lors de son exécution, vous obtiendrez la sortie suivante :

```
Hello John!
```

Il existe de nombreuses autres options et packages en Go pour lire les arguments de ligne de commande en fonction de vos besoins. N'hésitez pas à explorer et à expérimenter pour trouver ce qui convient le mieux à votre projet.

## Voir aussi

- La documentation officielle sur la fonction `os.Args` : https://golang.org/pkg/os/#Args
- La documentation officielle sur le package `flag` : https://golang.org/pkg/flag/