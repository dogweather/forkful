---
title:                "Lecture d'un fichier texte."
html_title:           "C++: Lecture d'un fichier texte."
simple_title:         "Lecture d'un fichier texte."
programming_language: "C++"
category:             "C++"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/cpp/reading-a-text-file.md"
---

{{< edit_this_page >}}

# Quoi & Pourquoi?

On entend souvent parler de la lecture de fichiers texte lorsqu'on parle de programmation, mais qu'est-ce que cela signifie exactement? En bref, la lecture d'un fichier texte consiste à ouvrir un fichier écrit en texte brut et à en extraire les données pour les utiliser dans un programme. Les programmeurs utilisent souvent cette méthode pour accéder à des données externes, telles que des fichiers de configuration, des bases de données ou des fichiers de données.

# Comment faire:

```C++
#include <iostream>
#include <fstream>

using namespace std;

int main() {
  ifstream file("exemple.txt"); // Ouverture du fichier en lecture seule
  string line = ""; // Déclaration d'une variable pour stocker chaque ligne
  while(getline(file, line)) { // Boucle pour lire toutes les lignes du fichier
    cout << line << endl; // Affichage de la ligne
  }
  file.close(); // Fermeture du fichier
  return 0;
}
```

**Exemple de contenu du fichier 'exemple.txt':**
```
Bonjour à tous!
Ceci est un exemple de fichier texte.
J'espère que cela vous aide à comprendre la lecture d'un fichier.
Au revoir!
```
**Sortie du programme:**
```
Bonjour à tous!
Ceci est un exemple de fichier texte.
J'espère que cela vous aide à comprendre la lecture d'un fichier.
Au revoir!
```

# Plongée en profondeur:

La lecture de fichiers texte est une méthode très ancienne en programmation, étant donné que les premiers programmes étaient souvent écrits en utilisant des fichiers texte comme moyen de stockage. Aujourd'hui, il existe d'autres méthodes plus avancées pour accéder à des données externes, telles que les bases de données ou les services de cloud. Cependant, la lecture de fichiers reste une méthode simple, rapide et pratique pour accéder aux données stockées localement.

# A voir également:

- [Documentation sur la lecture de fichiers en C++](https://www.cplusplus.com/doc/tutorial/files/)
- [Vidéo sur la lecture de fichiers en C++](https://www.youtube.com/watch?v=Iho2EdJgusQ)
- [Article sur l'importance de la lecture de fichiers en C++](https://www.journaldev.com/6022/cplusplus-file-handling-classes-write-read)