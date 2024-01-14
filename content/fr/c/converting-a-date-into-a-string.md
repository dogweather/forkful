---
title:                "C: Convertir une date en chaîne de caractères."
programming_language: "C"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/c/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Pourquoi

La conversion d'une date en chaîne de caractères est une fonctionnalité courante dans de nombreux programmes car elle permet de représenter une date de manière plus lisible pour les utilisateurs. Cela peut également faciliter la manipulation et la comparaison de dates dans le code.

## Comment faire

Pour convertir une date en chaîne de caractères en C, nous pouvons utiliser la fonction `strftime()` de la librairie `time.h`. Elle permet de formater une date selon un modèle spécifié et de la stocker dans un tableau de caractères.

```C
#include <stdio.h>
#include <time.h>

int main() {
    // Créer une struct tm avec une date spécifique
    struct tm date = {.tm_year = 2020, .tm_mon = 11, .tm_mday = 15}; 
    
    // Définir le modèle de formatage
    char format[] = "%d/%m/%Y";
    
    // Définir une taille suffisamment grande pour stocker la chaîne de caractères de la date
    char str_date[12];
    
    // Utiliser strftime pour convertir la date en chaîne
    strftime(str_date, 12, format, &date);
    
    // Afficher la date sous forme de chaîne
    printf("La date est : %s\n", str_date);
    
    return 0;
}
```

#### Output:
```
La date est : 15/11/2020
```

## Plongée en profondeur

La fonction `strftime()` utilise un modèle de formatage pour déterminer comment la date doit être représentée en chaîne de caractères. Ce modèle contient des balises spéciales qui seront remplacées par les valeurs correspondantes de la date. Par exemple, `%d` pour le jour du mois, `%m` pour le mois, `%Y` pour l'année complète, etc.

Il existe également des options pour modifier le format des valeurs, comme par exemple `%02d` pour afficher un nombre sur 2 chiffres, en ajoutant un zéro devant si nécessaire. Il est important de bien se référer à la documentation pour utiliser les bonnes balises et options en fonction de nos besoins.

## Voir aussi

- [La documentation de la fonction `strftime()`](https://www.tutorialspoint.com/c_standard_library/c_function_strftime.htm)
- [Un tutoriel détaillé sur la conversion de dates en C](https://www.coders-hub.com/2015/07/c-program-to-convert-date-into-string.html)