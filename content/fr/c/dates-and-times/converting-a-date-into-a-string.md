---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 17:53:53.888699-07:00
description: "Convertir une date en cha\xEEne de caract\xE8res en C implique de traduire\
  \ une structure de date ou un horodatage en un format lisible par l'homme. Les\u2026"
lastmod: '2024-03-13T22:44:58.384848-06:00'
model: gpt-4-0125-preview
summary: "Convertir une date en cha\xEEne de caract\xE8res en C implique de traduire\
  \ une structure de date ou un horodatage en un format lisible par l'homme."
title: "Convertir une date en cha\xEEne de caract\xE8res"
weight: 28
---

## Quoi et Pourquoi ?

Convertir une date en chaîne de caractères en C implique de traduire une structure de date ou un horodatage en un format lisible par l'homme. Les programmeurs effectuent souvent cette tâche pour afficher les dates dans des journaux, interfaces utilisateur, ou lorsqu'ils stockent des dates dans un format basé sur du texte comme JSON ou CSV.

## Comment faire :

La fonction `strftime` de la bibliothèque `<time.h>` est couramment utilisée à cette fin. Elle vous permet de formater la date et l'heure de diverses manières en spécifiant des indicateurs de format. Voici un exemple rapide :

```c
#include <stdio.h>
#include <time.h>

int main() {
    char dateStr[100];
    time_t now = time(NULL);
    struct tm *ptm = localtime(&now);

    // Convertir la date et l'heure en chaîne de caractères (par exemple, "Mer Jun 30 21:49:08 2021")
    strftime(dateStr, sizeof(dateStr), "%a %b %d %H:%M:%S %Y", ptm);
    
    printf("Date et Heure actuelles : %s\n", dateStr);
    return 0;
}
```

Le résultat pourrait ressembler à ceci :

```
Date et Heure actuelles : Mer Jun 30 21:49:08 2021
```

Vous pouvez personnaliser le format en changeant les indicateurs de format passés à `strftime`. Par exemple, pour obtenir la date au format `AAAA-MM-JJ`, vous utiliseriez `"%Y-%m-%d"`.

## Approfondissement

La fonction `strftime` et la bibliothèque `<time.h>` font partie de la Bibliothèque Standard C, qui remonte à la norme ANSI C originale (C89/C90). Bien qu'elle soit simple et prise en charge sur de nombreuses plateformes, cette approche peut sembler de bas niveau et encombrante par rapport aux langues de programmation modernes qui offrent des bibliothèques de dates et d'heures plus intuitives.

Il convient de noter, bien que les fonctions de temps de la bibliothèque standard C soient largement supportées et relativement simples à utiliser, elles manquent de certaines des fonctionnalités de manipulation de fuseaux horaires et d'internationalisation plus complexes trouvées dans les bibliothèques de langues plus récentes ou dans des bibliothèques C tierces telles que les Composants Internationaux pour Unicode (ICU).

Cependant, les capacités de personnalisation de la fonction `strftime` et son large support de plateforme en font un outil fiable et utile pour la conversion de chaînes de dates en C. Les programmeurs venant de langues avec des bibliothèques datetime de plus haut niveau peuvent devoir s'ajuster à sa nature de bas niveau mais la trouveront remarquablement puissante et polyvalente pour formater les dates et les heures pour une variété d'applications.
