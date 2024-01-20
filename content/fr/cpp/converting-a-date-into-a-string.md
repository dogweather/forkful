---
title:                "Convertir une date en chaîne de caractères"
html_title:           "Gleam: Convertir une date en chaîne de caractères"
simple_title:         "Convertir une date en chaîne de caractères"
programming_language: "C++"
category:             "C++"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/cpp/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi?
Convertir une date en chaîne est le processus de transformation d'une date en texte. Les programmeurs s'en servent pour rendre les dates plus lisibles et pour faciliter le stockage et l'échange de données de date dans divers formats.

## Comment faire:

Voici comment on fait en C++ :

```C++
#include <iostream>
#include <ctime>

int main() {
    time_t maintenant = time(0);
    char* dt = ctime(&maintenant);
    
    std::cout << "La date et l'heure locales sont : " << dt << std::endl;

    return 0;
}
```

Le code ci-dessus affichera quelque chose comme ça : `La date et l'heure locales sont : Wed Apr 7 12:34:56 2021`

## Plongée en profondeur

Le format de date et d'heure en C++ est basé sur la norme de l'heure et de la date UNIX, conçue pour rendre les dates et heures interopérables entre tous les systèmes d'exploitation. Une alternative à `ctime()` est `strftime()`, qui a beaucoup plus d'options de formatage.

Dans l’exemple ci-dessus, `ctime(&maintenant)` est une fonction qui convertit une variable `time_t` en format de date et d'heure. Cependant, c'est juste la pointe de l'iceberg car vous pourriez vouloir exploiter les bibliothèques spécifiques à C++ telles que `chrono` ou `date` qui offrent une plus grande flexibilité et une meilleure conformité avec les normes modernes.

## Voir aussi 

Voici certains liens connexes pour vous aider à comprendre plus sur ce sujet. 

1. Documentation de ctime: www.cplusplus.com/reference/ctime/
2. Documentation de strftime: www.cplusplus.com/reference/ctime/strftime/
3. Bibliothèque Chrono: en.cppreference.com/w/cpp/header/chrono
4. Bibliothèque Date (non-standard mais largement utilisée): github.com/HowardHinnant/date