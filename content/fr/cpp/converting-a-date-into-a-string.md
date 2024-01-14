---
title:    "C++: Transformer une date en chaîne de caractères"
keywords: ["C++"]
---

{{< edit_this_page >}}

## Pourquoi

La conversion d'une date en chaîne de caractères peut sembler être une tâche simple, mais elle est en fait utile pour afficher des données dans un format facilement compréhensible par les utilisateurs. Que vous souhaitiez afficher une date dans un journal, un rapport ou une interface utilisateur, la conversion en chaîne de caractères vous permettra de le faire de manière précise et efficace.

## Comment faire

Voici un exemple de code en C++ pour convertir une date en chaîne de caractères :

```C++
#include <iostream> 
#include <ctime> 

int main() 
{
    // Obtenir l'heure et la stocker dans une structure tm
    time_t now = time(0);
    tm *ltm = localtime(&now);

    // Utiliser la fonction strftime pour formater la date en chaîne de caractères
    char buffer[80];
    strftime(buffer, 80, "%d/%m/%Y", ltm);

    // Afficher la chaîne de caractères résultante
    std::cout << "La date est : " << buffer << std::endl;

    return 0;
}
```

La sortie de ce programme sera la date actuelle au format "jour/mois/année", par exemple "17/06/2021".

## Plongée en profondeur

Pour comprendre le processus de conversion d'une date en chaîne de caractères, il est important de connaître les différents composants d'une date. En C++, la structure tm contient neuf membres qui représentent la date et l'heure :

- tm_sec : le nombre de secondes écoulées depuis la minute précédente (entre 0 et 59)
- tm_min : le nombre de minutes écoulées depuis l'heure précédente (entre 0 et 59)
- tm_hour : le nombre d'heures écoulées depuis minuit (entre 0 et 23)
- tm_mday : le jour du mois (entre 1 et 31)
- tm_mon : le mois de l'année (entre 0 et 11)
- tm_year : le nombre d'années depuis 1900
- tm_wday : le jour de la semaine (dimanche = 0, samedi = 6)
- tm_yday : le jour de l'année (entre 0 et 365)
- tm_isdst : un indicateur pour savoir si l'heure d'été est en vigueur (0 si elle n'est pas en vigueur, une valeur positive si elle l'est)

En utilisant ces composants, la fonction strftime (qui se trouve dans la bibliothèque "ctime") formate ces éléments en une chaîne de caractères selon un modèle spécifié. Le modèle utilisé dans l'exemple "%d/%m/%Y" signifie "jour/mois/année", mais vous pouvez également utiliser d'autres modèles tels que "%H:%M:%S" pour afficher l'heure en format "heures:minutes:secondes". Pour plus d'informations sur les modèles disponibles, vous pouvez consulter la documentation de la fonction strftime.

## Voir aussi

- Documentation sur la fonction strftime de la bibliothèque ctime : https://www.cplusplus.com/reference/ctime/strftime/
- Tutoriel sur le formatage des dates en C++ : https://www.tutorialspoint.com/how-to-convert-date-to-string-in-cplusplus
- Vidéo expliquant la conversion d'une date en chaîne de caractères en C++ : https://www.youtube.com/watch?v=W5RuYU7WV7E