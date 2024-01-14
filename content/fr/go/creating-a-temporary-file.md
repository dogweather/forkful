---
title:    "Go: Création d'un fichier temporaire"
keywords: ["Go"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/go/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

# Pourquoi créer des fichiers temporaires en programmation Go ?

Créer des fichiers temporaires est une pratique courante en programmation, notamment en Go. Cela permet de stocker temporairement des données importantes pour un processus en cours d'exécution, sans encombrer le système avec des fichiers inutiles. C'est un moyen efficace de gérer les données de manière temporaire, sans risque d'interférer avec d'autres processus ou applications.

# Comment créer un fichier temporaire en Go ?

Pour créer un fichier temporaire en Go, il suffit d'utiliser la fonction "TempFile" du package "io/ioutil". Voici un exemple de code :

```Go
f, err := ioutil.TempFile("", "example")
if err != nil {
  panic(err)
}
defer os.Remove(f.Name())

fmt.Println("Nom du fichier temporaire :", f.Name())
 // code pour écrire dans le fichier temporaire
```

Dans cet exemple, nous utilisons la fonction "ioutil.TempFile" qui prend deux arguments : le premier est le répertoire dans lequel le fichier sera créé (ici, nous laissons une chaîne vide pour indiquer le répertoire temporaire par défaut) et le deuxième est le préfixe du nom du fichier temporaire.

Ensuite, nous utilisons la fonction "os.Remove" pour supprimer le fichier temporaire une fois que nous avons terminé d'utiliser ses données. Enfin, nous pouvons écrire dans le fichier temporaire en utilisant les fonctions d'écriture d'IO standard.

Lorsque nous exécutons ce code, nous obtenons un fichier temporaire avec un nom de la forme "exemple123456" (le nombre à la fin est généré de manière aléatoire). Le fichier sera automatiquement supprimé à la fin de l'exécution du programme.

# Plongez plus en profondeur dans la création de fichiers temporaires

Le package "io/ioutil" de Go offre également d'autres fonctions utiles pour la création de fichiers temporaires, telles que la fonction "TempDir" qui crée un répertoire temporaire au lieu d'un fichier.

Il est également possible de spécifier le répertoire dans lequel le fichier temporaire sera créé en utilisant la fonction "TempFile" avec un chemin de répertoire comme premier argument.

Il est important de noter que les fichiers temporaires ne sont pas automatiquement nettoyés par le système d'exploitation, même si le processus qui les a créés est terminé. Il est donc essentiel de les supprimer manuellement en utilisant la fonction "os.Remove" ou "os.RemoveAll" après avoir fini de les utiliser.

# Voir aussi

- Documentation officielle de Go sur la gestion des fichiers temporaires : https://golang.org/pkg/io/ioutil/#TempFile
- Tutoriel sur la création de fichiers temporaires en Go : https://gobyexample.com/temporary-files