---
title:                "Obtenir la date actuelle"
html_title:           "Bash: Obtenir la date actuelle"
simple_title:         "Obtenir la date actuelle"
programming_language: "C"
category:             "C"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/c/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi?
Obtenir la date courante dans une application C donne la date et l'heure actuelles. Les programmeurs l'utilisent pour générer des rapports liés à la date ou pour des fonctionnalités basées sur le temps.

## Comment faire:
Voici comment obtenir la date actuelle en C:
```C
//Inclure la bibliothèque time.h
#include<time.h>
 
int main(){
    //initialiser temp
    time_t temps;
    
    //obtenir le temps actuel
    temps = time(NULL);
 
    // Afficher la date et l'heure actuelles
    printf("%s", ctime(&temps));
    
    return 0;
}
```
La sortie ressemblerait à ceci: `Mon Mar 15 01:16:46 2021`

## Plongée profonde
Historiquement, en C, la date et l'heure actuelles ont toujours été obtenues à l'aide de la bibliothèque `time.h`. 
Alternativement, vous pouvez utiliser `gettimeofday()` or `clock_gettime()`. Cependant, `time(NULL)` est plus portable et plus précis jusqu'à la seconde. 
Cela fonctionne en renvoyant le nombre actuel de secondes écoulées depuis l'époque UNIX (00:00:00 UTC, le 1 janvier 1970), pas compter les secondes bissextiles.

## Voir aussi
Pour plus d'informations, vous pouvez consulter ces ressources:
- Document de la bibliothèque C Standard, Section 7.27: [Time.h](http://www.open-std.org/JTC1/SC22/WG14/www/docs/n1256.pdf)
- Tutorial C: [Manipulation de date et d'heure](https://www.codingunit.com/c-tutorial-the-time-date-and-calendar-functions)
- L'Epoch Unix: [Explication](https://www.epochconverter.com/)