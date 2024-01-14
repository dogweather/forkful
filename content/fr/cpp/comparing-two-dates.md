---
title:                "C++: Comparer deux dates"
simple_title:         "Comparer deux dates"
programming_language: "C++"
category:             "C++"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/cpp/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Pourquoi 

La comparaison de deux dates est une tâche courante en programmation et peut être très utile dans de nombreuses situations. Cela peut permettre de vérifier la validité des données saisies par l'utilisateur, de trier des données temporelles ou encore de planifier des événements.

## Comment faire

Pour comparer deux dates en C++, vous pouvez utiliser la fonction `difftime()` qui calcule la différence en secondes entre deux dates passées en paramètres. Voici un exemple de code:

```C++
#include <iostream>
#include <ctime>

using namespace std;

int main() {
    // définition de deux dates
    time_t date1 = time(NULL);
    time_t date2 = time(NULL);

    // utilisation de la fonction difftime
    double diff = difftime(date1, date2);

    // affichage du résultat
    cout << "La différence entre les deux dates est de " << diff << " secondes." << endl;

    return 0;
}
```

Dans cet exemple, nous utilisons la fonction `time()` pour obtenir la date actuelle à l'aide de l'objet `time_t`. Ensuite, nous passons ces deux dates à la fonction `difftime()` et stockons le résultat dans une variable `double`. Enfin, nous affichons le résultat à l'écran.

Si les deux dates sont identiques, le résultat sera égal à 0. Dans le cas contraire, il sera soit positif si la première date est antérieure à la deuxième, soit négatif si elle est postérieure.

## Approfondissement

Lors de la comparaison de dates, il existe plusieurs éléments à prendre en compte. Par exemple, les dates peuvent être représentées de différentes manières selon le format choisi, et les fuseaux horaires peuvent également avoir un impact sur les résultats.

De plus, il peut être intéressant d'utiliser des fonctions telles que `gmtime()` ou `localtime()` pour obtenir des informations plus précises sur les dates, telles que le jour de la semaine ou le numéro de la semaine.

Il est également important de faire attention aux erreurs de calcul qui peuvent survenir en raison des années bissextiles ou des changements d'heure.

Enfin, il est recommandé de consulter la documentation de votre compilateur ou de votre bibliothèque standard pour connaître les fonctions disponibles pour comparer les dates et les différentes options de formatage.

## Voir aussi

- [Documentation de la fonction difftime en C++](https://www.cplusplus.com/reference/ctime/difftime/)
- [Documentation de la fonction time en C++](https://www.cplusplus.com/reference/ctime/time/)
- [Documentation de la structure tm en C++](https://www.cplusplus.com/reference/ctime/tm/)