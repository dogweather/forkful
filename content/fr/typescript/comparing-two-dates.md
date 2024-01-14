---
title:    "TypeScript: Comparaison de deux dates"
keywords: ["TypeScript"]
---

{{< edit_this_page >}}

## Pourquoi

Comparer des dates est souvent une tâche courante dans la programmation, que ce soit pour vérifier si une date est antérieure ou postérieure à une autre, ou pour calculer le nombre de jours entre deux dates. Dans cet article, nous allons découvrir comment comparer facilement deux dates avec TypeScript.

## Comment faire

Pour comparer deux dates avec TypeScript, nous allons utiliser les objets Date. Supposons que nous ayons deux dates à comparer, "8 mars 2021" et "10 juin 2021". Nous allons les définir comme objets Date dans notre code :

```TypeScript
const date1 = new Date("2021-03-08");
const date2 = new Date("2021-06-10");
```

Maintenant que nous avons nos deux dates, nous pouvons utiliser les opérateurs de comparaison standard pour les comparer :

```TypeScript
if (date1 < date2) {
  console.log("La date 1 est antérieure à la date 2.");
} else if (date1 > date2) {
  console.log("La date 1 est postérieure à la date 2.");
} else {
  console.log("Les deux dates sont identiques.");
}
```

Dans cet exemple, nous utilisons les opérateurs < (inférieur), > (supérieur) et === (égal) pour comparer les deux dates. Nous pouvons également utiliser les opérateurs <= (inférieur ou égal) et >= (supérieur ou égal).

Nous pouvons également utiliser la méthode getTime() pour comparer des dates en millisecondes :

```TypeScript
if (date1.getTime() < date2.getTime()) {
  console.log("La date 1 est antérieure à la date 2.");
} else if (date1.getTime() > date2.getTime()) {
  console.log("La date 1 est postérieure à la date 2.");
} else {
  console.log("Les deux dates sont identiques.");
}
```

## Deep Dive

Il est important de noter que les dates sont comparées en fonction de leur horodatage, et non en fonction de leur format d'affichage. De plus, les dates peuvent varier selon les fuseaux horaires, il est donc recommandé de toujours travailler avec des dates heure-locale.

Pour calculer le nombre de jours entre deux dates, nous pouvons utiliser la méthode getTime() pour obtenir leur différence en millisecondes, puis la convertir en jours :

```TypeScript
const difference = Math.abs(date1.getTime() - date2.getTime());
const jours = Math.floor(difference / (1000 * 60 * 60 * 24));
```

## Voir aussi

- [Documentation officielle de TypeScript sur les objets Date](https://www.typescriptlang.org/docs/handbook/dates-and-times.html)
- [Tutoriel sur la comparaison de dates en JavaScript](https://www.digitalocean.com/community/tutorials/how-to-compare-dates-in-javascript)
- [Article sur la manipulation de dates en TypeScript](https://blog.bitsrc.io/manipulating-dates-with-moment-js-and-typescript-714e24d644c2)