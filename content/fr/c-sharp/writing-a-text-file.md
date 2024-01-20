---
title:                "Écrire un fichier texte"
html_title:           "C#: Écrire un fichier texte"
simple_title:         "Écrire un fichier texte"
programming_language: "C#"
category:             "C#"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/c-sharp/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Qu'est-ce que c'est & pourquoi ?

Écrire un fichier texte signifie simplement écrire du texte sur un fichier, plutôt que de l'écrire dans un environnement de développement. Les programmeurs font cela pour stocker des données, des configurations ou des informations spécifiques sur leur programme, de manière à les utiliser ultérieurement.

## Comment faire :

```C#
// Créer un nouveau fichier texte
File.Create("nom-du-fichier.txt");

// Écrire du texte sur un fichier existant
File.WriteAllText("nom-du-fichier.txt", "Bonjour le monde!");

// Ajouter du texte à un fichier existant
File.AppendAllText("nom-du-fichier.txt", "Ceci est un exemple de texte ajouté.");

// Lire le contenu d'un fichier texte
string contenu = File.ReadAllText("nom-du-fichier.txt");
```

## Plongée en profondeur :

L'écriture d'un fichier texte peut sembler une tâche banale pour les programmeurs d'aujourd'hui, mais cela a été un énorme sujet de discussion dans les premiers jours de la programmation informatique. Les alternatives telles que l'utilisation d'une base de données pour stocker des données ou l'écriture d'un fichier binaire nécessitant un traitement spécial n'ont pas toujours été disponibles.

Les programmeurs peuvent également trouver utile d'utiliser des flots de données (streams) pour écrire sur un fichier plutôt que d'utiliser la classe File présentée ci-dessus.

## Voir aussi :

- [Code Envato Tuts sur l'écriture de fichiers en C#](https://code.tutsplus.com/articles/file-io-in-c-101--cms-32815)
- [Tutoriel sur les flots de données en C#](https://csharp-station.com/Tutorial/CSharp/Lesson23)