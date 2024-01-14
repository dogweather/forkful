---
title:                "C#: Écrire un fichier texte"
programming_language: "C#"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/c-sharp/writing-a-text-file.md"
---

{{< edit_this_page >}}

Pourquoi écrire un fichier texte en programmation C#?

Écrire un fichier texte peut être une tâche utile en programmation C#. Cela peut vous aider à stocker et à manipuler des données en utilisant du texte brut, ce qui peut être plus facile à gérer que d'autres formats de données. Dans cet article, nous allons découvrir comment écrire un fichier texte en utilisant du code C#.

Comment faire:

Pour écrire un fichier texte en C#, vous devez utiliser la classe File et la méthode AppendAllText (). Voici un exemple de code qui peut vous aider à écrire un fichier texte:

```C#
// Définition du chemin du fichier
string cheminFichier = @"C:\MonFichier.txt";

// Écriture du texte dans le fichier
File.AppendAllText(cheminFichier, "Bonjour le monde!");
```

Ceci va créer un fichier texte nommé "MonFichier.txt" dans votre dossier C: avec le texte "Bonjour le monde!" écrit à l'intérieur.

Pour ajouter du contenu supplémentaire dans ce fichier, vous pouvez utiliser à nouveau la méthode AppendAllText() en passant le chemin du fichier et le nouveau texte à ajouter.

```C#
// Ajouter du texte supplémentaire
File.AppendAllText(cheminFichier, "Comment ça va?");
```

Cela va ajouter la phrase "Comment ça va?" à la fin du fichier texte.

Plutôt que d'ajouter du texte, vous pouvez également utiliser la méthode AppendAllLines() pour ajouter un tableau de lignes à votre fichier texte. Voici un exemple de code:

```C#
// Définition du tableau de lignes à ajouter
string[] nouvellesLignes = { "Je vais bien, merci!", "Et toi?" };

// Ajouter les lignes au fichier
File.AppendAllLines(cheminFichier, nouvellesLignes);
```

Cela va ajouter les deux nouvelles lignes à la fin du fichier texte.

Plutôt que d'ajouter du contenu à la fin du fichier, vous pouvez également utiliser la méthode WriteAllText() pour écrire du nouveau contenu dans le fichier et écraser tout ce qui était déjà présent. Voici un exemple de code:

```C#
// Écraser le contenu et écrire du nouveau texte
string nouveauTexte = "Ceci est un nouveau texte";
File.WriteAllText(cheminFichier, nouveauTexte);
```

Cela va remplacer tout le contenu du fichier texte avec la phrase "Ceci est un nouveau texte".

Plongée en profondeur:

Maintenant que nous avons vu comment écrire un fichier texte en utilisant du code C#, vous pouvez explorer d'autres possibilités offertes par la classe File et les différentes méthodes disponibles. Vous pouvez également utiliser la classe StreamWriter pour avoir plus de contrôle sur la manière dont vous écrivez du texte dans le fichier.

Voir aussi:

- Documentation de Microsoft sur la classe File en C# : https://docs.microsoft.com/fr-fr/dotnet/api/system.io.file?view=net-5.0
- Tutoriel sur l'écriture et la lecture de fichiers texte en C# : https://www.codeproject.com/Articles/4157322/Csharp-Read-and-Write-Text-Files-in-a-Few-Simple-L

Nous espérons que cet article vous a été utile pour comprendre comment écrire un fichier texte en programmation C#. N'hésitez pas à explorer d'autres options et à pratiquer pour mieux maîtriser cette tâche importante en développement de logiciels.