---
title:    "Javascript: Comparaison de deux dates"
keywords: ["Javascript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/javascript/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Pourquoi

Comparer deux dates est un élément fondamental de la programmation en Javascript. C'est particulièrement utile lorsque vous avez besoin de vérifier si une date est antérieure ou postérieure à une autre, ou si elles sont les mêmes. Cela peut être utile pour la gestion de données, la création de rappels et bien d'autres choses encore. Dans cet article, nous allons vous montrer comment comparer deux dates en Javascript.

## Comment faire

Pour comparer deux dates en Javascript, il faut utiliser l'objet Date et ses méthodes. Tout d'abord, nous devons créer deux variables contenant nos dates à comparer :

```Javascript
let date1 = new Date(2021, 6, 20); // 20 Juillet 2021
let date2 = new Date(2021, 10, 15); // 15 Novembre 2021
```

Ensuite, nous pouvons utiliser la méthode `getTime()` pour obtenir le nombre de millisecondes écoulées depuis le 1er Janvier 1970 pour chaque date. Nous pouvons ensuite les comparer en utilisant des opérateurs de comparaison tels que `<`, `>`, `===`. Voici un exemple :

```Javascript
if(date1.getTime() < date2.getTime()){
    console.log("La date 1 est antérieure à la date 2");
} else if(date1.getTime() > date2.getTime()){
    console.log("La date 1 est postérieure à la date 2");
} else if(date1.getTime() === date2.getTime()){
    console.log("Les deux dates sont identiques");
}
```

Dans cet exemple, nous comparons les deux dates en utilisant la méthode `getTime()` et affichons le résultat en fonction du résultat de la comparaison.

## Plongée en profondeur

Il est important de noter que si les dates sont égales, elles peuvent toujours être différentes en termes de formatage. Par exemple, si nous comparons une date au format "mm/jj/aaaa" avec une autre date au format "jj/mm/aaaa", elles peuvent sembler égales, mais elles ne le seront pas car elles ne sont pas dans le même format. Il est donc important de formater correctement les dates avant de les comparer pour obtenir des résultats précis.

De plus, les dates peuvent également être affectées par le paramètre de fuseau horaire, il est donc important de tenir compte de cela lors de la comparaison de dates dans différents endroits.

## Voir aussi

Pour en savoir plus sur la comparaison de dates en Javascript, vous pouvez consulter les ressources suivantes :

- [Documentation MDN sur l'objet Date en Javascript](https://developer.mozilla.org/fr/docs/Web/JavaScript/Reference/Objets_globaux/Date)
- [Guide ultime de la gestion des dates en Javascript](https://blog.usejournal.com/the-ultimate-guide-to-date-manipulation-in-javascript-3d90bed5493d)

J'espère que cet article vous a été utile pour comprendre comment comparer deux dates en Javascript. Bonne programmation !