---
title:                "Convertir une date en chaîne de caractères"
html_title:           "Gleam: Convertir une date en chaîne de caractères"
simple_title:         "Convertir une date en chaîne de caractères"
programming_language: "C"
category:             "C"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/c/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Qu'est-ce que c'est et pourquoi?

Convertir une date en chaîne de caractères en C consiste à transformer une valeur de date en texte lisible. Les programmeurs le font pour faciliter le formatage, le stockage et l'affichage des données de date.

## Comment faire:

Le code suivant utilise la bibliothèque `time.h` pour obtenir la date et l'heure courantes, puis `strftime` pour convertir cette information en une chaîne de caractères.

```C
#include <time.h>
#include <stdio.h>

int main() {
    char buffer[80];
    time_t temps_courant = time(NULL);
    strftime(buffer, 80, "%d-%m-%Y %H:%M:%S", localtime(&temps_courant));
    
    printf("Date et heure courantes : %s\n", buffer);
    
    return 0;
}
```
Lorsque vous exécutez ce code, il produira une sortie comme celle-ci:

```C
Date et heure courantes : 28-09-2022 12:30:15
```

## Approfondissement

Historiquement, la méthode traditionnelle en C pour convertir une date en chaîne de caractères est `asctime` de 'time.h', mais elle est moins flexible que `strftime`.

Il existe des solutions alternatives, par exemple les bibliothèques `boost` et `datetime` en C++, mais elles peuvent être excessives pour des tâches simples.

L'utilisation de `strftime` est la méthode privilégiée car elle offre une grande flexibilité dans le formatage de la date et de l'heure. Elle prend un tampon pour le résultat, sa taille, une chaîne de formatage et une structure `struct tm`.

## Voir aussi

Pour plus d'informations sur `strftime` et les structures de date et d'heure en C, consultez les liens suivants:

- [Spécification C99, section 7.23](http://www.open-std.org/jtc1/sc22/wg14/www/docs/n1256.pdf)
- [Bibliothèque GNU C, Date and Time](https://www.gnu.org/software/libc/manual/html_node/Date-and-Time.html)
- [Manuel Linux `strftime`](https://man7.org/linux/man-pages/man3/strftime.3.html)