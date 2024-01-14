---
title:    "Bash: Calculer une date dans le futur ou le passé"
keywords: ["Bash"]
---

{{< edit_this_page >}}

## Pourquoi

Calculer la date dans le passé ou dans le futur peut être un outil très utile dans de nombreuses situations. Par exemple, planifier des événements, suivre les délais ou simplement savoir quel jour de la semaine tombera une certaine date.

## Comment faire

Pour calculer une date dans le passé ou dans le futur, nous pouvons utiliser la commande `date` en Bash. Voici un exemple pour calculer la date dans 5 jours :

```
Bash date -d "5 days"
```

L'ajout du préfixe `-d` nous permet de préciser à la commande `date` que nous voulons calculer une date dans le futur. Nous pouvons également spécifier une date de départ en utilisant la même syntaxe, par exemple si nous voulons calculer la date dans 5 jours à partir du 10 octobre 2021 :

```
Bash date -d "2021-10-10 + 5 days"
```

La sortie sera au format standard de date, mais nous pouvons également utiliser des options pour modifier le format de la sortie. Par exemple, pour avoir la date au format JJ/MM/AAAA :

```
Bash date -d "5 days" +"%d/%m/%Y"
```

## Plongée en profondeur

La commande `date` est en réalité beaucoup plus puissante et peut calculer des dates dans le passé ou dans le futur avec une précision allant jusqu'au millième de seconde. Elle prend en compte les années bissextiles et les fuseaux horaires. Elle peut même calculer des dates à partir de chaînes de caractères relatives, comme "next Thursday" ou "last month".

Pour une utilisation plus avancée, nous pouvons également utiliser la commande `date` avec des variables et des fonctions pour automatiser le calcul de dates dans des scripts Bash.

## Voir aussi

- [Documentation officielle de la commande date](https://www.gnu.org/software/coreutils/manual/html_node/date-invocation.html#date-invocation)
- [Tutoriel sur le calcul de dates en Bash](https://www.ostechnix.com/calculate-date-in-linux/)
- [Autres astuces Bash sur notre blog](https://notreblogfrench.com/tag/bash/)