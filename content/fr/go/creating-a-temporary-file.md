---
title:                "Go: Création d'un fichier temporaire"
simple_title:         "Création d'un fichier temporaire"
programming_language: "Go"
category:             "Go"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/go/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Pourquoi

Créer un fichier temporaire peut être utile dans diverses situations en programmation Go. Cela peut être nécessaire pour stocker temporairement des données ou pour effectuer des opérations qui ne nécessitent pas de fichiers permanents.

## Comment faire

Pour créer un fichier temporaire en Go, vous pouvez utiliser la fonction `ioutil.TempFile` qui prend en paramètre le répertoire de stockage temporaire et le préfixe du nom du fichier. Voici un exemple de code :

```
fichierTemporaire, err := ioutil.TempFile("", "monfichiertemp_")
if err != nil {
    log.Fatal(err)
}
defer os.Remove(fichierTemporaire.Name())

fmt.Println("Nom du fichier temporaire :", fichierTemporaire.Name())
```

Dans cet exemple, nous utilisons une chaîne vide comme premier argument pour spécifier que le fichier sera stocké dans le répertoire temporaire par défaut de l'OS. Le préfixe du nom du fichier est spécifié dans la deuxième chaîne. Ensuite, nous utilisons la fonction `os.Remove` pour supprimer le fichier temporaire une fois que nous n'en avons plus besoin.

Lors de l'exécution de ce code, vous devriez voir une sortie similaire à ceci :

```
Nom du fichier temporaire : /tmp/monfichiertemp_147657390
```

## Plongée en profondeur

La fonction `ioutil.TempFile` crée un fichier et le retourne en tant qu'objet `*os.File`. Cela signifie que vous pouvez effectuer toutes les opérations de fichier standard sur ce fichier temporaire, telles que l'écriture et la lecture de données.

Il est également possible de spécifier un autre répertoire de stockage temporaire en utilisant le premier argument de la fonction `ioutil.TempFile`. Si vous souhaitez utiliser un répertoire spécifique, assurez-vous qu'il existe et qu'il a les permissions d'écriture appropriées.

## Voir aussi

- [Documentation officielle de Go pour la fonction TempFile](https://golang.org/pkg/io/ioutil/#TempFile)
- [Article Medium sur la création de fichiers temporaires en Go](https://medium.com/@lmvicente1986/creating-temporary-files-in-go-24d42f38927b)