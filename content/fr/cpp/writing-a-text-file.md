---
title:                "C++: Écrire un fichier texte"
programming_language: "C++"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/cpp/writing-a-text-file.md"
---

{{< edit_this_page >}}

Bonjour à tous ! Si vous êtes un passionné de programmation, alors vous savez déjà à quel point le fait d'écrire un fichier texte peut être utile dans de nombreux cas. Dans cet article, nous allons explorer le processus de création d'un fichier texte en utilisant le langage de programmation C++. De la création du fichier à son écriture et sa lecture, nous allons tout couvrir pour vous permettre de maîtriser cet aspect essentiel de la programmation. Alors allons-y !

## Pourquoi

Avant de plonger dans les détails techniques, parlons rapidement de pourquoi il est utile et important de savoir écrire un fichier texte en tant que programmeur. Les fichiers texte sont un moyen simple et efficace de stocker des données de manière permanente. Ils peuvent être utilisés pour enregistrer les préférences de l'utilisateur, les résultats de calculs ou tout autre type de données nécessaires à votre programme. Savoir comment créer et gérer efficacement des fichiers texte vous permettra d'ajouter une couche supplémentaire de flexibilité à vos applications.

## Comment faire

Maintenant que nous avons compris l'importance de savoir écrire un fichier texte, passons à l'étape pratique. Voici un exemple de code en C++ pour créer et écrire dans un fichier texte :

```C++
#include <iostream>
#include <fstream> 
// Inclure les bibliothèques nécessaires

using namespace std;

int main()
{
  ofstream fichier("monfichier.txt"); 
  // Créer un objet de type ofstream et ouvrir le fichier "monfichier.txt"

  if (fichier.is_open())
  {
    fichier << "Bienvenue dans mon fichier texte !"; 
    // Écrire du texte dans le fichier

    fichier.close(); 
    // Fermer le fichier
  }
  else
  {
    cout << "Erreur lors de l'ouverture du fichier";
  }

  return 0;
}
```

Vous remarquerez que nous avons utilisé les bibliothèques `<iostream>` et `<fstream>` dans notre code. La première est utilisée pour les entrées/sorties de données tandis que la seconde est spécifiquement utilisée pour les opérations de lecture et d'écriture sur des fichiers. Ensuite, nous avons créé un objet de type `ofstream` en lui fournissant le nom de notre fichier comme paramètre. Ensuite, nous avons utilisé la fonction `is_open()` pour vérifier si le fichier a été ouvert avec succès avant d'écrire notre texte à l'aide de l'opérateur `<<`. Enfin, n'oubliez pas de fermer le fichier une fois que vous avez terminé. Et voilà, vous venez de créer un fichier texte en C++ !

## Plongée en profondeur

Maintenant que vous avez une compréhension de base du processus de création d'un fichier texte en C++, jetons un coup d'œil à quelques autres concepts importants à retenir.

### Mode d'ouverture

Lorsque vous utilisez la fonction `ofstream` pour ouvrir un fichier, vous pouvez spécifier un mode d'ouverture en utilisant le deuxième paramètre de la fonction. Le mode par défaut est `ios::out`, qui ouvre le fichier en mode écriture. D'autres modes, tels que `ios::app` pour ajouter du contenu au fichier existant ou `ios::trunc` pour effacer le contenu existant avant l'écriture, peuvent également être spécifiés.

### Lecture de fichiers

Pour lire le contenu d'un fichier texte en C++, vous utilisez un objet `ifstream` au lieu d'un `ofstream`. Le processus est similaire, mais vous utilisez la fonction `getline()` pour lire chaque ligne du fichier plutôt que d'utiliser l'opérateur `<<`.

### Erreurs

Lorsque vous travaillez avec des fichiers texte, il est important de garder à l'esprit que des erreurs peuvent survenir lors de la lecture et de l'écriture. Il est donc recommandé d'utiliser les fonctions `fail()` ou `bad()` pour vérifier si une opération a été effectuée avec succès.

## Voir aussi

Voilà donc les bases pour créer et gérer des fichiers texte en C++. Pour en savoir plus sur les opérations de lecture et d'écriture de fichiers, vous pouvez consulter ces ressources :

- [Tutoriel cplusplus.com sur les fichiers](http://www.cplusplus.com/doc/tutorial/files