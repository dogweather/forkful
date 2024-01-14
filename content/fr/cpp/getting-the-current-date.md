---
title:    "C++: Obtenir la date actuelle"
keywords: ["C++"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/cpp/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Pourquoi Obtenir la Date Actuelle

Obtenir la date actuelle peut sembler être un petit détail dans la programmation, mais c'est une fonctionnalité très utile à avoir dans votre code. Non seulement elle peut aider à suivre l'ordre d'exécution des tâches, mais elle peut également être utilisée pour afficher des informations contextuelles dans votre programme, comme la date de création d'un fichier ou la date de dernière modification. Dans cet article, nous allons vous expliquer comment obtenir la date actuelle en utilisant le langage de programmation C++.

## Comment Obtenir la Date Actuelle

Pour obtenir la date actuelle en C++, nous allons utiliser la bibliothèque standard <ctime> qui contient des fonctions pour la manipulation du temps et des dates. Tout d'abord, nous devons inclure cette bibliothèque dans notre programme en utilisant la directive #include.

```C++
#include <ctime>
```

Ensuite, nous allons créer une variable de type time_t qui va stocker le nombre de secondes écoulées depuis le 1er janvier 1970. Cela sera notre point de référence pour calculer la date actuelle.

```C++
time_t now = time(0);
```

Ensuite, nous allons utiliser la fonction ctime() pour convertir notre variable de type time_t en une chaîne de caractères représentant la date actuelle. Nous allons stocker cette chaîne de caractères dans une variable appelée current_date.

```C++
char* current_date = ctime(&now);
```

Et enfin, nous allons afficher la date actuelle en utilisant la fonction cout de la bibliothèque standard <iostream>.

```C++
std::cout << "La date actuelle est : " << current_date << std::endl;
```

Lorsque nous exécutons ce code, nous obtenons le résultat suivant :

```
La date actuelle est : Tue Mar 16 12:05:14 2021
```

## Plongée Profonde

Il est important de noter que la date et l'heure affichées dépendront du fuseau horaire de votre machine. Si vous souhaitez obtenir la date actuelle dans un fuseau horaire spécifique, vous pouvez utiliser la fonction localtime() en passant en paramètre l'adresse de votre variable time_t et le fuseau horaire voulu.

```C++
char* current_date = asctime(localtime(&now));
```

Vous pouvez également formater la date à votre guise à l'aide de la fonction strftime(). Cette dernière prend en paramètres différentes options et renvoie une chaîne de caractères formatée selon ces options.

```C++
char* formatted_date;
strftime(formatted_date, 25, "%d/%m/%Y - %H:%M", localtime(&now));

std::cout << "La date actuelle au format jour/mois/année - heure:minute est : " << formatted_date << std::endl;
```

Lorsque nous exécutons ce code, nous obtenons le résultat suivant :

```
La date actuelle au format jour/mois/année - heure:minute est : 16/03/2021 - 12:05
```

## Voir Aussi

- [Documentation de la bibliothèque <ctime> en C++](https://www.cplusplus.com/reference/ctime/)
- [Tutoriel sur la manipulation de dates et de temps en C++](https://www.geeksforgeeks.org/c-programming-date-time-reference/)
- [Site de référence sur le langage C++](https://www.cplusplus.com/)