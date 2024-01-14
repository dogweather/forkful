---
title:    "C++: Vérification de l'existence d'un répertoire"
keywords: ["C++"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/cpp/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

Pourquoi: Vous vous demandez peut-être pourquoi vous devriez vous intéresser à vérifier si un répertoire existe dans votre programme C++. Eh bien, la réponse est simple: en tant que programmeur, il est important de toujours anticiper toutes les possibilités et de s'assurer que votre code fonctionne correctement dans toutes les situations. Vérifier si un répertoire existe est une étape importante dans cette démarche.

Comment faire: Pour vérifier si un répertoire existe dans un programme C++, il existe plusieurs méthodes, mais la plus simple consiste à utiliser la fonction « std::filesystem::exists ». Voici un exemple de code:

```C++
#include <iostream>
#include <filesystem>

using namespace std;

int main() {
    // Définir le chemin du répertoire à vérifier
    filesystem::path chemin = filesystem::current_path() / "documents";
    
    //Vérifier si le répertoire existe
    if(filesystem::exists(chemin)) {
        cout << "Le répertoire existe !" << endl;
    } else {
        cout << "Le répertoire n'existe pas." << endl;
    }
    
    return 0;
}
```

Output:

```
Le répertoire existe !
```

Plongée plus profonde: Maintenant que nous avons vu un exemple simple de vérification de l'existence d'un répertoire, il est temps d'explorer un peu plus en détail ce qui se passe réellement. La fonction « exists » fait partie de la bibliothèque standard C++17 « filesystem » qui fournit des outils pour gérer les fichiers et les répertoires. Elle prend en entrée le chemin du fichier ou du répertoire à vérifier et renvoie un booléen indiquant si l'élément existe ou non. Si le chemin passé en paramètre est un lien symbolique, la fonction vérifiera l'existence du lien et non de la cible réelle.

Pour rendre le code plus robuste, il est également possible de vérifier le type de l'élément avec la fonction « is_directory » avant d'appeler la fonction « exists ». Cela garantit que nous ne vérifions que les répertoires et non les fichiers réguliers.

Vérifier l'existence d'un répertoire peut également être utile pour s'assurer que nous avons les autorisations nécessaires pour y accéder ou pour vérifier si un répertoire est vide avant de le supprimer.

Voir aussi:
- [Documentation sur la bibliothèque standard C++17 « filesystem »](https://en.cppreference.com/w/cpp/filesystem)
- [Tutoriel sur la gestion des fichiers et des répertoires en C++](https://www.learncpp.com/cpp-tutorial/186-basic-file-io/)
- [Article sur la manipulation de fichiers et de répertoires en C++](https://www.geeksforgeeks.org/file-handling-c-classes/)
- [Guide de référence rapide pour la bibliothèque standard C++17 « filesystem »](https://www.modernescpp.com/index.php/cpp17-the-filesystem-library)
- [Vidéo sur la gestion des fichiers et des répertoires en C++](https://www.youtube.com/watch?v=D9ptA0IteOA)

Voir également: Pour plus d'informations sur la manipulation des fichiers et des répertoires en C++, n'hésitez pas à consulter les ressources mentionnées ci-dessus ou à explorer d'autres articles et tutoriels sur le sujet. En tant que programmeur, il est important de bien comprendre ces concepts afin de créer des programmes robustes et efficaces.