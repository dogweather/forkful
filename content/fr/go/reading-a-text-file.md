---
title:                "Go: Lecture d'un fichier texte"
simple_title:         "Lecture d'un fichier texte"
programming_language: "Go"
category:             "Go"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/go/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Pourquoi

Si vous êtes un développeur en herbe ou que vous cherchez à apprendre un nouveau langage de programmation, alors cet article est fait pour vous ! Nous allons plonger dans le monde de la programmation en Go et apprendre comment lire un fichier texte. Cette compétence est essentielle pour tout programmeur, car elle vous permettra de manipuler des données plus facilement, notamment pour des tâches telles que l'analyse de données ou la gestion de fichiers de configuration.

## Comment faire

La première étape pour lire un fichier texte en Go consiste à ouvrir le fichier à l'aide de la fonction ```os.Open()```. Cette fonction prend en paramètre le chemin vers le fichier et renvoie un pointeur de type ```*os.File```.

Ensuite, vous pouvez utiliser le pointeur de fichier pour lire les données à l'aide de la méthode ```Read()```. Cette méthode prendra un tableau de bytes en paramètre et renverra le nombre de bytes lus à partir du fichier.

Voici un exemple de code pour lire un fichier texte en Go :

```
fichier, err := os.Open("monfichier.txt")
if err != nil {
    log.Fatal(err)
}
defer fichier.Close()

// Création d'un tableau de bytes pour stocker les données lues
donnees := make([]byte, 1024)

// Lecture du fichier et stockage des données dans le tableau de bytes
nbBytes, err := fichier.Read(donnees)
if err != nil {
    log.Fatal(err)
}

fmt.Println("Nombre de bytes lus :", nbBytes)
fmt.Println("Contenu du fichier :", string(donnees))
```

Voici ce que pourrait être le contenu du fichier texte que nous avons lu :

```
Bonjour à tous !
Ceci est un fichier texte de test.
Je vous souhaite une excellente journée.
```

Et voici le résultat de notre code après l'exécution :

```
Nombre de bytes lus : 78
Contenu du fichier : Bonjour à tous !
Ceci est un fichier texte de test.
Je vous souhaite une excellente journée.
```

## Plongée en profondeur

Il est important de noter que la méthode ```Read()``` peut renvoyer une erreur si elle ne parvient pas à lire tous les bytes du fichier. Dans ce cas, vous devrez utiliser une boucle pour lire le fichier jusqu'à la fin.

De plus, n'oubliez pas de fermer le pointeur de fichier à l'aide de la méthode ```Close()``` après avoir terminé de lire le fichier. Cela libèrera les ressources utilisées par le fichier.

## Voir aussi

Si vous souhaitez en savoir plus sur la lecture de fichiers en Go, voici quelques liens utiles :

- [Documentation officielle de Go sur l'ouverture et la lecture de fichiers](https://golang.org/pkg/os/#File)
- [Tutoriel vidéo sur la lecture de fichiers en Go](https://www.youtube.com/watch?v=OhLhWvG3Dpo)
- [Exemples de lecture de fichiers en Go sur GitHub](https://github.com/dstotijn/go-simple-file-reader/blob/master/file-reader.go)

Maintenant que vous savez comment lire un fichier texte en Go, vous êtes prêt à vous plonger dans le monde passionnant de la programmation en Go. Bonne chance et amusez-vous bien !