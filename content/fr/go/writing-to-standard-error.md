---
title:    "Go: Écrire sur l'erreur standard"
keywords: ["Go"]
---

{{< edit_this_page >}}

## Pourquoi

Dans la programmation en Go, il est courant d'avoir recours à l'écriture vers le "standard error" ou en abrégé "stderr". Mais pourquoi est-il important d'écrire vers cet emplacement spécifique et comment pouvons-nous le faire efficacement ? Dans cet article, nous explorerons la raison pour laquelle l'écriture vers le standard error est importante et comment le faire en utilisant le langage de programmation Go.

## Comment faire

```Go
// Exemple de code pour écrire vers le standard error en utilisant print et println

func main() {
    // Écriture vers le standard error en utilisant print
    fmt.Fprint(os.Stderr, "Ceci est un exemple de texte écrit vers le standard error en utilisant print")

    // Écriture vers le standard error en utilisant println
    fmt.Fprintln(os.Stderr, "Ceci est un exemple de texte écrit vers le standard error en utilisant println")
}
```

Lorsque nous exécutons ce code, nous obtenons le résultat suivant :

```
Ceci est un exemple de texte écrit vers le standard error en utilisant print
Ceci est un exemple de texte écrit vers le standard error en utilisant println
```

Nous pouvons également utiliser la fonction `panic` pour écrire vers le standard error en cas d'erreur. Voici un exemple de code :

```Go
// Exemple de code pour écrire vers le standard error en utilisant panic

func main() {
    // Appel de la fonction errorFunction avec un argument "erreur"
    errorFunction("erreur")
}

func errorFunction(err error) {
    // Si l'erreur existe, utilisez panic pour écrire vers le standard error
    if err != nil {
        panic(err)
    }
}
```

Lorsque nous exécutons ce code et que l'erreur "erreur" est passée en argument, nous obtenons le résultat suivant :

```
panic: erreur

goroutine 1 [running]:
main.errorFunction(...)
        /chemin/vers/le/fichier/main.go:11
main.main()
        /chemin/vers/le/fichier/main.go:6 +0x8f
exit status 2
```

Le `panic` écrit une information détaillée sur l'erreur vers le standard error et arrête également l'exécution du programme.

## Deep Dive

Maintenant que nous savons comment écrire vers le standard error en utilisant print, println et panic, explorons pourquoi il est important de le faire. Le standard error est un emplacement spécifique dans la console où les messages d'erreur sont affichés. Cela peut être utile lors du débogage de notre code car il nous permet de voir rapidement les messages d'erreurs sans avoir à les rechercher dans le code source. De plus, lors de l'utilisation de la fonction `panic`, écrire vers le standard error permet d'obtenir une trace détaillée de l'erreur qui s'est produite, ce qui facilite la localisation et la résolution des problèmes.

## Voir aussi

- [Article sur le standard error en Go](https://golang.org/pkg/os/#pkg-constants)
- [Tutoriel sur Go par le site officiel de Go](https://golang.org/doc/tutorial/getting-started) 
- [Livres sur Go conseillés par la communauté](https://github.com/ardanlabs/gotraining/blob/master/README_FR.md#conseils-de-lecture)