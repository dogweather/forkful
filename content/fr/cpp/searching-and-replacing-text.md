---
title:                "C++: Recherche et remplacement de texte"
simple_title:         "Recherche et remplacement de texte"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/cpp/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Pourquoi

Remplacer du texte est une tâche courante en programmation, en particulier lorsqu'il s'agit de modifier de grandes quantités de données. La recherche et le remplacement de texte peuvent vous aider à automatiser cette tâche fastidieuse et à économiser du temps et des efforts précieux.

## Comment Faire

Pour effectuer une recherche et un remplacement de texte en C++, vous aurez besoin d'une fonction qui accepte le texte à rechercher et à remplacer, ainsi que le nouveau texte qui remplacera l'ancien. Voici un exemple de code qui illustre le processus:

```C++
#include <string> // inclure la bibliothèque string

using namespace std; // utiliser l'espace de nom standard

// fonction pour remplacer le texte
void searchAndReplace(string& originalText, string searchText, string replaceText) {
  size_t index = 0; // initialiser un index pour la position du premier caractère du texte à remplacer
  
  // boucle tant que l'index est inférieur à la longueur du texte original
  while ((index = originalText.find(searchText, index)) != string::npos) {
    // remplacer le texte à partir de l'index avec le nouveau texte
    originalText.replace(index, searchText.length(), replaceText);
    
    // mettre à jour l'index pour passer au prochain texte à remplacer
    index += replaceText.length();
  } 
}

int main() {
  // définir un texte original
  string texteOriginal = "Bonjour, je m'appelle Pierre. Comment ça va ?";

  // rechercher le mot "Pierre" et le remplacer par "Jean"
  searchAndReplace(texteOriginal, "Pierre", "Jean");

  // afficher le nouveau texte
  cout << texteOriginal << endl;

  return 0;
}

// Output:
// Bonjour, je m'appelle Jean. Comment ça va ?
```

Dans cet exemple, nous utilisons la fonction `string::find()` pour trouver la position du texte à remplacer dans le texte original. Ensuite, nous utilisons la fonction `string::replace()` pour remplacer le texte à partir de cette position avec le nouveau texte. Enfin, nous mettons à jour l'index pour passer au prochain texte à remplacer jusqu'à ce que tous les mots recherchés aient été remplacés.

## Plongée Approfondie

En plus de la fonction `string::find()` et `string::replace()`, il existe d'autres fonctions utiles pour rechercher et remplacer du texte en C++. Par exemple, `string::compare()` peut être utilisé pour comparer deux chaînes de caractères et `string::erase()` pour supprimer une partie du texte. Vous pouvez également utiliser des expressions régulières pour une recherche et un remplacement plus complexes.

Il est également important de prendre en compte les performances lors de la recherche et du remplacement de texte en C++. Il est recommandé d'utiliser l'opérateur `+=` pour concaténer le texte au lieu de la fonction `string::append()` qui affecte la performance.

Enfin, il est important de comprendre comment fonctionnent les fonctions de recherche et de remplacement de texte et d'éviter les erreurs courantes telles que la modification du texte original pendant la recherche.

## Voir Aussi

- [Documentation C++ sur les fonctions de recherche et de remplacement de texte](https://www.cplusplus.com/reference/string/string/)

- [Guide du C++ pour les débutants](https://formations.dymense.com/cours-c-cpp/index.php)

- [Tutoriel vidéo sur la recherche et le remplacement de texte en C++](https://www.youtube.com/watch?v=en0afb87rSY)

Merci d'avoir lu cet article sur la recherche et le remplacement de texte en C++! J'espère que cela vous a été utile dans vos projets de programmation. N'hésitez pas à consulter les liens recommandés pour en savoir plus sur le sujet. À bientôt!