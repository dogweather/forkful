---
title:    "C: Conversion d'une date en chaîne de caractères"
keywords: ["C"]
---

{{< edit_this_page >}}

## Pourquoi

Il y a plusieurs raisons pour lesquelles on pourrait vouloir convertir une date en une chaîne de caractères dans un programme en C. Par exemple, cela pourrait être utile pour afficher une date sur une interface utilisateur ou pour traiter des données horodatées.

## Comment faire

Pour convertir une date en une chaîne de caractères en utilisant le langage C, nous pouvons utiliser la fonction `strftime` de la bibliothèque standard `time.h`. Cette fonction prend en entrée une chaîne de format et une structure `tm` représentant la date. Voici un exemple de code :

```C
#include <stdio.h>
#include <time.h>

int main(){
    // création de la structure tm avec la date actuelle
    time_t temps;
    struct tm *dt;
    time(&temps);
    dt = localtime(&temps);

    // conversion de la date en une chaîne de caractères
    char strdate[15];
    strftime(strdate, 15, "%d/%m/%Y", dt);

    // affichage du résultat
    printf("La date du jour est : %s\n", strdate);

    return 0;
}
```

Voici la sortie correspondante lorsque le code est exécuté :

```
La date du jour est : 21/09/2021
```

Dans cet exemple, nous avons utilisé le format de date `"%d/%m/%Y"` pour obtenir la date sous la forme `jour/mois/année`. Il existe de nombreux autres formats disponibles, tels que `"%H:%M"` pour l'heure au format 24 heures et `"%I:%M %p"` pour l'heure au format 12 heures.

## Plongée en profondeur

Il est important de noter que la fonction `strftime` utilise les paramètres de langue et de localisation définis dans le système pour formater la date. Cela peut entraîner des différences dans les résultats selon les configurations des différents systèmes.

De plus, la structure `tm` peut être utilisée pour représenter une date dans une plage plus large que celles prises en charge par `strftime`. Par exemple, la structure peut stocker des informations sur le fuseau horaire et les années bissextiles, alors que `strftime` les ignore.

Il est donc crucial de bien comprendre les paramètres de langue et de localisation ainsi que les possibilités de la structure `tm` pour choisir la meilleure méthode de conversion de date en chaîne de caractères.

## Voir aussi

- [Documentation de la fonction `strftime` (en anglais)](https://en.cppreference.com/w/c/chrono/strftime)
- [Guide de la bibliothèque `time.h` (en français)](https://openclassrooms.com/fr/courses/19980-apprenez-a-programmer-en-c/16860-utilisez-les-fonctions-de-gestion-du-temps)

Nous espérons que cet article vous a été utile dans votre apprentissage de la conversion de date en chaîne de caractères en utilisant le langage C. N'hésitez pas à consulter les ressources recommandées pour approfondir vos connaissances sur le sujet. Bon codage !