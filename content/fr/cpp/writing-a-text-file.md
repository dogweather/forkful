---
title:                "C++: Ecrire un fichier texte"
simple_title:         "Ecrire un fichier texte"
programming_language: "C++"
category:             "C++"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/cpp/writing-a-text-file.md"
---

{{< edit_this_page >}}

# Pourquoi écrire un fichier texte en C++?

Il existe de nombreuses raisons pour lesquelles on pourrait vouloir utiliser le langage de programmation C++ pour écrire un fichier texte. Certains pourraient vouloir stocker des données dans un format facilement lisible, tandis que d'autres pourraient utiliser un fichier texte pour enregistrer les logs ou les rapports de leur programme. Dans tous les cas, écrire un fichier texte en C++ est une compétence utile pour tout programmeur.

## Comment le faire?

La première étape pour écrire un fichier texte en C++ est de créer un objet `ofstream` en utilisant la bibliothèque `fstream`. Cet objet représente le fichier que vous allez écrire. Ensuite, utilisez la méthode `open()` pour spécifier le chemin du fichier et choisir le mode d'écriture. Par exemple, si vous voulez écrire dans un fichier appelé "mon_fichier.txt" en mode ajout, vous pouvez utiliser le code suivant:

```C++
#include <iostream>
#include <fstream>
  
int main() 
{ 
    // créer un objet ofstream 
    std::ofstream fichier; 
  
    // ouvrir le fichier en mode ajout 
    fichier.open("mon_fichier.txt", std::ios::app); 
  
    // écrire dans le fichier 
    fichier << "Bonjour le monde!" << std::endl; 
  
    // fermer le fichier 
    fichier.close(); 
  
    return 0; 
} 
```

Ce code va ajouter la phrase "Bonjour le monde!" à la fin du fichier "mon_fichier.txt". Vous pouvez également utiliser les modes "trunc" pour écraser le contenu du fichier, ou "out" pour écrire à la fin mais sans ajouter de caractère de fin de ligne.

## Approfondissement

Lors de l'écriture d'un fichier texte en C++, il est important de prendre en compte certains éléments tels que l'encodage du texte, le format du fichier et la manipulation des erreurs. Vous pouvez utiliser la bibliothèque `fstream` pour spécifier l'encodage du texte que vous souhaitez utiliser, ainsi que pour gérer les erreurs potentielles lors de l'ouverture ou de l'écriture du fichier. N'hésitez pas à consulter la documentation en ligne pour plus d'informations sur l'utilisation de cette bibliothèque ou sur les différentes options de manipulation des fichiers texte en C++.

## Voir aussi

- [Documentation sur la bibliothèque `fstream` en C++](https://fr.cppreference.com/w/cpp/header/fstream)
- [Guide pratique pour écrire un fichier texte en C++](https://www.enseignement.polytechnique.fr/informatique/INF441/INF441b/html/c++/textio.html)
- [GitHub - Exemples de manipulation de fichiers en C++](https://github.com/cpp-examples/cpp-examples/tree/master/file-io)