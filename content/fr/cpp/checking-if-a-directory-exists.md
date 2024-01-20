---
title:                "Vérifier si un répertoire existe"
html_title:           "C++: Vérifier si un répertoire existe"
simple_title:         "Vérifier si un répertoire existe"
programming_language: "C++"
category:             "C++"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/cpp/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Qu'est-ce et pourquoi?

Vérifier si un répertoire existe est une tâche courante: il s'agit de confirmer la présence d'un dossier spécifique dans le système de fichiers de l'ordinateur. Les programmeurs font cela pour éviter les erreurs lors de la manipulation des fichiers, par exemple en essayant d'écrire dans un dossier inexistant.

## Comment faire:

Nous utiliserons la bibliothèque `filesystem` introduite dans C++17. Voici un exemple:

```C++
#include <filesystem>
#include <iostream>
 
int main() {
    std::string chemin = "/chemin/vers/le/dossier";
    
    if(std::filesystem::exists(chemin)) {
        std::cout << "Le répertoire existe.\n";
    } else {
        std::cout << "Le répertoire n'existe pas.\n";
    }

    return 0;
}
```

Dans cet exemple, changez la valeur de la variable `chemin` à votre choix. Le programme vous dira si le répertoire existe ou non.

## Plongée profonde

Historiquement, vérifier si un répertoire existe n'était pas si facile en C++. Avant C++17, il fallait utiliser des appels de système spécifiques à la plate-forme, ce qui rendait le code difficile à porter.

Il y a des alternatives comme `boost::filesystem` de la bibliothèque Boost, mais cela exige l'installation de Boost. Maintenant, avec `std::filesystem` en C++17, cette tâche est beaucoup plus simple et portable.

Côté mise en œuvre, `std::filesystem::exists` interroge le système de fichiers de l'ordinateur pour vérifier l'existence du chemin. Cela peut échouer si le programme n'a pas les permissions nécessaires.

## Voir aussi

- [Documentation C++ sur filesystem](http://www.cplusplus.com/reference/filesystem/)
- [Un aperçu de la bibliothèque Boost et de Boost::filesystem](https://www.boost.org/doc/libs/1_75_0/libs/filesystem/doc/index.htm)
- [Comment définir les permissions d'accès aux fichiers en C++](https://en.cppreference.com/w/cpp/filesystem/perms)