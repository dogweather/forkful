---
title:                "Convertir une date en chaîne de caractères"
html_title:           "Gleam: Convertir une date en chaîne de caractères"
simple_title:         "Convertir une date en chaîne de caractères"
programming_language: "Bash"
category:             "Bash"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/bash/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Qu'est-ce & Pourquoi?
Convertir une date en chaîne de caractères permet de manipuler et d'afficher cette date sous différentes formes. Les programmeurs le font pour faciliter la comparaison de dates, l'internationalisation et la personnalisation de l'affichage d'une date.

## Comment faire:
Bash fournit la commande `date` pour ça. 

Voici quelques exemples:

Pour obtenir la date actuelle :
```
Bash
$ date
```

Format de sortie par défaut : 
```
Mar 15 Mar 2022 12:34:56 UTC
```

Pour obtenir la date sous forme de chaîne de caractères :
```
Bash
$ date +"%m/%d/%Y"
```

Format de sortie : 
```
03/15/2022
```

## Plongée en profondeur
Historiquement, avant UNIX et POSIX, convertir une date en chaîne de caractères était un processus difficile et complexe. Ces normes ont défini la commande `date`, simplifiant considérablement cette tâche.

Il existe d'autres alternatives comme strftime() en C et des méthodes dans de nombreux autres langages de programmation.

En ce qui concerne les détails de l'implémentation, la commande `date` utilise l'heure système pour obtenir l'heure actuelle, puis formatte cette date en fonction des paramètres qu'elle reçoit. C'est un outil très flexible qui permet d'obtenir la date et l'heure sous presque n'importe quelle forme.

## Voir aussi
- man date : Pour plus d'informations, consultez [la page du manuel date](https://man7.org/linux/man-pages/man1/date.1.html)
- strftime() : Plus sur cette alternative C sur [cette page](https://www.cplusplus.com/reference/ctime/strftime/)
- DateTime en Python : Pour une approche Python, consultez [cette ressource](https://docs.python.org/3/library/datetime.html#strftime-and-strptime-behavior)
- Date en JavaScript : Pour une version JavaScript, regardez [ici](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date/toLocaleDateString)