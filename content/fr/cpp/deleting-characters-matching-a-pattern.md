---
title:                "Suppression de caractères correspondant à un motif"
html_title:           "C++: Suppression de caractères correspondant à un motif"
simple_title:         "Suppression de caractères correspondant à un motif"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/cpp/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Pourquoi

Supprimer des caractères correspondant à un modèle peut être utile dans de nombreuses situations, telles que nettoyer des données ou filtrer du texte pour une analyse ultérieure.

## Comment faire

Voici un exemple de code en C++ montrant comment supprimer des caractères correspondant à un modèle :

```C++
#include <iostream>
#include <string>

using namespace std;

int main() {
    // Créer une chaîne de caractères avec un modèle à supprimer
    string texte = "Supprimer tous les voyelles";

    // Parcourir la chaîne de caractères et supprimer chaque voyelle
    for (int i = 0; i < texte.length(); i++) {
        if (texte[i] == 'a' || texte[i] == 'e' || texte[i] == 'i' || texte[i] == 'o' || texte[i] == 'u') {
            texte.erase(i, 1);
        }
    }

    // Afficher le résultat
    cout << texte << endl;

    return 0;
}

// Output : Sprmr t s ls vylls
```

## Plongée en profondeur

En C++, la fonction `string::erase()` permet de supprimer des caractères d'une chaîne de caractères. Elle prend en paramètre la position à partir de laquelle les caractères doivent être supprimés et le nombre de caractères à supprimer. Dans notre exemple, la fonction `erase()` est utilisée à l'intérieur d'une boucle `for` pour parcourir la chaîne de caractères et supprimer les voyelles. Il est important de noter que la fonction `erase()` modifie la chaîne de caractères d'origine, donc il est conseillé de créer une copie de cette dernière si elle doit être réutilisée par la suite.

## Voir aussi

- [Documentation de la fonction `erase()` en C++](https://www.cplusplus.com/reference/string/string/erase/)
- [Guide complet sur les chaînes de caractères en C++](https://www.tutorialspoint.com/cplusplus/cpp_strings.htm)
- [Autres opérations de manipulation de chaînes de caractères en C++](https://www.geeksforgeeks.org/c-string-manipulation-with-examples/)