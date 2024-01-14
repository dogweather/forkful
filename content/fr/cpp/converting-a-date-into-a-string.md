---
title:    "C++: Conversion d'une date en chaîne de caractères"
keywords: ["C++"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/cpp/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Pourquoi

La conversion d'une date en chaîne de caractères est une tâche courante en programmation. Elle permet de présenter des dates de manière plus lisible pour les utilisateurs et facilite le stockage et la manipulation des données temporelles dans un programme. Dans cet article, nous allons voir comment effectuer cette conversion en utilisant le langage de programmation C++.

## Comment faire

Pour convertir une date en chaîne de caractères en C++, nous allons utiliser la classe `stringstream` de la bibliothèque standard. Cette classe permet de créer des objets de type `string` à partir de valeurs de différents types de données. Voici un exemple de code qui convertit la date actuelle en une chaîne de caractères au format "jour/mois/année" :

```C++
#include <iostream>
#include <sstream>
#include <ctime>

int main() {
    // Obtenir la date actuelle
    time_t now = time(0);
    tm *date = localtime(&now);
    
    // Convertir la date en chaîne de caractères
    std::stringstream ss;
    ss << date->tm_mday << "/" << date->tm_mon + 1 << "/" << date->tm_year + 1900;
    std::string date_string = ss.str();
    
    // Afficher la date convertie
    std::cout << date_string << std::endl;
    
    return 0;
}
```

La sortie de ce programme sera quelque chose comme : `4/3/2021`, selon la date actuelle.

Il est également possible d'utiliser la fonction `strftime()` de la bibliothèque `ctime` pour formater directement la date en utilisant un modèle de chaîne de caractères. Voici un exemple qui affiche la date actuelle au format "jour de la semaine, jour mois année" :

```C++
#include <iostream>
#include <ctime>

int main() {
    // Obtenir la date actuelle
    time_t now = time(0);
    tm *date = localtime(&now);
    
    // Définir le modèle de chaîne de caractères
    char format[] = "%A, %d %B %Y";
    
    // Stocker la date formatée dans une chaîne de caractères
    char date_string[50];
    strftime(date_string, sizeof(date_string), format, date);
    
    // Afficher la date formatée
    std::cout << date_string << std::endl;
    
    return 0;
}
```

La sortie sera par exemple : `jeudi, 4 mars 2021`.

## Plongée en profondeur

En convertissant une date en chaîne de caractères, il est important de prendre en compte le format que vous souhaitez utiliser. La bibliothèque `ctime` offre plusieurs modèles de chaîne de caractères pour formater la date selon vos besoins. Vous pouvez également créer votre propre modèle en utilisant les balises de formatage spécifiques disponibles dans la documentation de la bibliothèque.

De plus, il est important de noter que la conversion d'une date en chaîne de caractères peut varier en fonction du système d'exploitation et de la localisation. Il est donc important de tester votre code sur différentes plateformes pour vous assurer que votre programme fonctionne correctement.

## Voir aussi

- [Documentation de `stringstream` en C++ (en anglais)](https://www.cplusplus.com/reference/sstream/stringstream/)
- [Documentation de `strftime()` en C (en anglais)](https://www.cplusplus.com/reference/ctime/strftime/)