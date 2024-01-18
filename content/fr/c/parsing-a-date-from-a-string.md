---
title:                "Analyser une date à partir d'une chaîne de caractères"
html_title:           "C: Analyser une date à partir d'une chaîne de caractères"
simple_title:         "Analyser une date à partir d'une chaîne de caractères"
programming_language: "C"
category:             "C"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/c/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## Qu'est-ce que c'est et pourquoi? 
Parsing une date à partir d'une chaîne est le processus de convertir une date représentée sous forme de chaîne de caractères en un format compréhensible par l'ordinateur. Les programmeurs utilisent cette technique pour manipuler et comparer des dates dans leurs programmes.

## Comment:
Voici un exemple simple de code en C pour parser une date à partir d'une chaîne :

```C
#include <stdio.h>
#include <time.h>

int main()
{
    char date[10] = "15/07/2021";
    
    struct tm parsed;
    
    // Utilisation de la fonction strptime pour parser la date
    strptime(date, "%d/%m/%Y", &parsed);
    
    // Affichage du jour, mois, année
    printf("Jour: %d\n", parsed.tm_mday);
    printf("Mois: %d\n", parsed.tm_mon + 1);
    printf("Année: %d\n", parsed.tm_year + 1900);
    
    return 0;
}
```

Résultat :

```
Jour: 15
Mois: 7
Année: 2021
```
Ce code utilise la fonction `strptime` de la bibliothèque `time.h` pour parser la date. Le format `%d/%m/%Y` indique que la date est représentée comme le jour, mois et année séparés par des barres obliques.

## Plongée en profondeur:
Parsing de dates à partir de chaînes n'est pas une nouveauté en informatique. En fait, il a été introduit dès les premières versions du langage C. Une alternative à la fonction `strptime` est `scanf` qui peut également être utilisée pour parser des dates en utilisant un format spécifique.

Cependant, avec l'évolution des langages et des bibliothèques, il existe maintenant des outils plus avancés et plus précis pour effectuer cette tâche. Par exemple, la bibliothèque `date.h` incluse dans le langage C11 offre des fonctions pour manipuler les dates et les heures de manière plus simple et fiable.

En termes d'implémentation, le processus de parsing de dates peut être plus complexe lorsque différents formats de date sont utilisés ou lorsqu'il y a des erreurs dans la chaîne de caractères représentant la date. Il est donc important pour les programmeurs de comprendre les différentes méthodes et fonctions disponibles pour s'assurer que les dates sont correctement parseées dans leur programme.

## Voir aussi:
- [Documentation officielle de la fonction strptime](https://www.gnu.org/software/libc/manual/html_node/Low_002dLevel-Time-String-Parsing.html)
- [Bibliothèque date.h pour le langage C11](https://en.wikipedia.org/wiki/Date_and_time_functions_(C))