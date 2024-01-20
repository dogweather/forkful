---
title:                "Analyser une date à partir d'une chaîne"
html_title:           "Clojure: Analyser une date à partir d'une chaîne"
simple_title:         "Analyser une date à partir d'une chaîne"
programming_language: "C++"
category:             "C++"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/cpp/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?
Analyser une date à partir d'une chaîne de caractères consiste à convertir un texte contenant des informations de date (par exemple, "31/12/2021") dans un format de date informatique compréhensible. Les programmeurs le font pour faciliter le traitement, l'affichage et la sauvegarde des informations de date.

## Comment faire :
En C++, vous pouvez utiliser la bibliothèque `chrono` pour analyser une date à partir d'une chaîne de caractères.


```C++
#include <chrono>
#include <sstream>

int main() {
    std::istringstream ss("2022/01/01 12:00:00");

    std::chrono::system_clock::time_point tp;
    ss >> date::parse("%Y/%m/%d %H:%M:%S", tp);

    auto time_t = std::chrono::system_clock::to_time_t(tp);

    std::cout << std::ctime(&time_t) << std::endl;
    return 0;
}
```

Le résultat de ce code sera `Sat Jan  1 12:00:00 2022`.


## Plongeons plus profondément :
Historiquement, l'analyse des dates à partir de chaînes de caractères a été gérée de différentes manières en C++. Avant `chrono`, on avait tendance à utiliser `strptime` ou `sscanf`, mais ces fonctions avaient leurs limites et n'étaient pas robustes.

Il existe plusieurs alternatives à la fonction `chrono`, telles que la bibliothèque `date` de Howard Hinnant ou la bibliothèque `boost::date`.

En termes de détails d'implémentation, la fonction `date::parse` fonctionne en analysant la chaîne de date/heure à partir de l'entrée d'après un format spécifié. Les spécificateurs de format sont les mêmes que ceux de `strftime` et `strptime`.

## Voir aussi :
Pour approfondir vos connaissances et explorer des alternatives, voici quelques liens utiles :

1. [Bibliothèque de dates de Howard Hinnant](https://github.com/HowardHinnant/date)
2. [Bibliothèque Boost Date_Time](https://www.boost.org/doc/libs/1_75_0/doc/html/date_time.html)
3. [Documentation C++ sur la bibliothèque Chrono](https://fr.cppreference.com/w/cpp/chrono)