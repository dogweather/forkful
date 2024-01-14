---
title:    "C++: Écrire un fichier texte"
keywords: ["C++"]
---

{{< edit_this_page >}}

## Pourquoi

Ecrire dans un fichier texte peut sembler être une tâche simple et banale, mais il peut en fait être très utile dans de nombreux cas. Que ce soit pour sauvegarder des données, générer des rapports ou simplement stocker des informations pour une utilisation ultérieure, écrire dans un fichier texte est un concept de base en programmation qui peut vous être très utile.

## Comment faire

Pour écrire dans un fichier texte en C++, nous allons utiliser la bibliothèque standard `<fstream>`. Tout d'abord, nous devons déclarer un objet de type `ofstream` qui sera utilisé pour ouvrir et écrire dans le fichier. Ensuite, nous allons utiliser la méthode `open()` pour indiquer le nom du fichier que nous voulons créer et ouvrir.

Dans l'exemple ci-dessous, nous allons créer un fichier texte appelé "monfichier.txt" et y écrire quelques lignes de texte :

```C++
#include <iostream>
#include <fstream>

using namespace std;

int main() {
    ofstream monfichier("monfichier.txt");

    if(monfichier.is_open()) {
        monfichier << "Ceci est une ligne de texte." << endl;
        monfichier << "Et ceci en est une autre." << endl;
        monfichier.close();
    }
    else cout << "Erreur lors de l'ouverture du fichier.";

    return 0;
}
```
Ce code va créer un fichier texte dans le même dossier que votre programme contenant les deux lignes de texte suivantes :

```
Ceci est une ligne de texte.
Et ceci en est une autre.
```

## Plongée en profondeur

Maintenant que nous avons vu comment écrire dans un fichier texte en utilisant `<fstream>`, il est important de comprendre quelques concepts clés. Tout d'abord, la méthode `open()` peut être utilisée pour ouvrir un fichier en mode "écriture" (`ofstream`), "lecture" (`ifstream`) ou les deux (`fstream`). Si le fichier spécifié n'existe pas, il sera créé automatiquement. Si le fichier existe déjà, son contenu sera écrasé par défaut. Si vous souhaitez ajouter du contenu à un fichier existant, vous pouvez spécifier le mode "append" en utilisant la méthode `open()` suivante :

```C++
ofstream monfichier("monfichier.txt", ios::app);
```

De plus, il est important de noter que toutes les données écrites dans un fichier texte sont considérées comme des chaînes de caractères. Si vous souhaitez écrire des données de type autre que `string`, vous devez utiliser les méthodes de conversion appropriées. Par exemple, pour écrire un entier dans un fichier, vous pouvez utiliser la méthode `to_string()` :

```C++
int nombre = 42;
monfichier << to_string(nombre) << endl;
```

## Voir aussi

Maintenant que nous savons comment écrire dans un fichier texte en C++, voici quelques liens utiles pour en apprendre plus sur ce sujet :

- https://www.cplusplus.com/doc/tutorial/files/
- https://www.tutorialspoint.com/cplusplus/cpp_files_streams.htm
- https://www.geeksforgeeks.org/writing-data-to-a-text-file-in-cpp/

Merci d'avoir lu cet article sur l'écriture d'un fichier texte en C++ ! J'espère qu'il vous sera utile dans vos projets futurs. N'hésitez pas à explorer davantage les liens suggérés pour approfondir vos connaissances sur ce sujet. Bonne programmation !