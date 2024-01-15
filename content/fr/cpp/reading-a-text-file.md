---
title:                "Lecture d'un fichier texte"
html_title:           "C++: Lecture d'un fichier texte"
simple_title:         "Lecture d'un fichier texte"
programming_language: "C++"
category:             "C++"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/cpp/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Pourquoi

Si vous êtes nouveau dans le monde de la programmation en C++, vous vous demandez peut-être pourquoi vous devriez apprendre à lire un fichier texte. La réponse est simple : la lecture de fichiers texte est l'une des tâches les plus courantes en programmation et est souvent nécessaire pour traiter des données externes dans un programme.

## Comment faire

Pour ouvrir et lire un fichier texte en C++, vous aurez besoin de la bibliothèque standard <fstream>. Elle contient deux classes : ifstream pour lire un fichier et ofstream pour écrire dans un fichier. Voici un exemple de code pour ouvrir un fichier texte appelé "mon_fichier.txt" :

```C++
#include <iostream>
#include <fstream>

using namespace std;

int main() {
  ifstream myfile; // déclaration de l'objet ifstream
  myfile.open ("mon_fichier.txt"); // ouverture du fichier
  if (!myfile) { // vérifie si l'ouverture a échoué
    cout << "Impossible d'ouvrir le fichier." << endl;
  }
  else {
    cout << "Le fichier a été ouvert avec succès !" << endl;
    // lecture du contenu du fichier
    string line;
    while (getline(myfile, line)) {
      cout << line << endl;
    }
    myfile.close(); // fermeture du fichier
  }
  return 0;
}
```

Output :

```
Le fichier a été ouvert avec succès !
Ceci est une ligne écrite dans le fichier.
Et voici une autre ligne.
```

## Plongée en profondeur

Il est important de comprendre que la lecture d'un fichier texte en C++ se fait ligne par ligne. La fonction getline() récupère une ligne complète du fichier et la stocke dans une variable de type "string". Cependant, si vous avez besoin de récupérer des données spécifiques dans une ligne, il existe des méthodes telles que la fonction find() pour rechercher une sous-chaîne de caractères ou encore la fonction stoi() pour convertir une chaîne de caractères en nombre entier.

De plus, il est également possible d'utiliser des opérateurs de flux pour lire directement des données d'un fichier, comme dans l'exemple suivant :

```C++
ifstream myfile;
myfile.open("mon_fichier.txt");
int number;
myfile >> number; // lit la première valeur numérique du fichier
string text;
myfile >> text; // lit la prochaine valeur sous forme de chaîne de caractères
```

## Voir aussi

- [Documentation de la bibliothèque <fstream> en français](https://www.cplusplus.com/reference/fstream/?setlocale=fr)
- [Tutoriel vidéo sur la lecture de fichiers en C++](https://www.youtube.com/watch?v=hTK8Y-s2fww)
- [Exemples pratiques de lecture de fichiers en C++](https://www.codespeedy.com/read-file-line-by-line-in-cpp/)