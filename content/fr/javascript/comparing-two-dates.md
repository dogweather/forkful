---
title:                "Javascript: Comparaison de deux dates"
programming_language: "Javascript"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/javascript/comparing-two-dates.md"
---

{{< edit_this_page >}}

# Pourquoi comparer deux dates en JavaScript?

Comparer deux dates en JavaScript est une pratique courante dans le développement web. Cela permet de vérifier si une date est antérieure ou postérieure à une autre, ou encore de calculer la différence entre deux dates. Dans cet article, nous allons vous montrer comment comparer efficacement deux dates en utilisant du code JavaScript.

## Comment le faire?

Tout d'abord, il est important de comprendre que les dates en JavaScript sont représentées par des objets Date. Pour comparer deux dates, nous allons donc utiliser les méthodes et propriétés de ces objets.

Voici un exemple de code qui compare deux dates:

```Javascript
let date1 = new Date(); //crée une date correspondant à la date et à l'heure actuelles
let date2 = new Date("2021-01-01"); //crée une date correspondant au 1er janvier 2021

//comparaison des dates en utilisant les méthodes getTime() et getTimezoneOffset()
if(date1.getTime() > date2.getTime()){
    console.log("La date 1 est plus récente que la date 2");
}
else if(date1.getTime() === date2.getTime()){
    console.log("Les deux dates sont identiques");
}
else{
    console.log("La date 2 est plus récente que la date 1");
}

//affichage de la différence entre les deux dates en nombre de jours
let difference = Math.abs(date1.getTime() - date2.getTime());
let jours = difference / (1000 * 3600 * 24);
console.log(`Il y a ${jours} jours entre les deux dates`);
```

Dans cet exemple, nous utilisons les méthodes getTime() pour obtenir le temps en millisecondes depuis le 1er janvier 1970, et getTimezoneOffset() pour prendre en compte les éventuels décalages horaires. Ensuite, nous utilisons des conditions pour déterminer si une date est antérieure, postérieure ou identique à l'autre, ainsi que la méthode abs() pour calculer la différence entre les deux dates.

## Plongée en profondeur

Il est également possible de comparer des dates en utilisant des opérateurs de comparaison (>, <, ===). Cependant, cela peut poser des problèmes à cause des décalages horaires et de la façon dont JavaScript traite les objets Date. Il est donc recommandé d'utiliser les méthodes getTime() et getTimezoneOffset () pour une comparaison plus précise.

De plus, il est important de noter que les mois en JavaScript commencent à partir de 0, donc janvier est représenté par 0, février par 1, et ainsi de suite. Cela peut causer des erreurs lors de la comparaison de dates si on ne fait pas attention.

Une autre chose à prendre en compte est que certaines méthodes de comparaison peuvent renvoyer une valeur en heures, en minutes ou en secondes, selon les paramètres choisis. Il est donc important de bien lire la documentation officielle de JavaScript afin de comprendre le comportement de chaque méthode et de l'adapter en conséquence dans votre code.

Enfin, il est toujours recommandé de tester votre code avec différentes dates et scénarios pour vous assurer qu'il fonctionne correctement dans toutes les situations.

## Voir aussi

- [Documentation officielle sur les objets Javascript Date](https://developer.mozilla.org/fr/docs/Web/JavaScript/Reference/Objets_globaux/Date)
- [Différentes façons de comparer des dates en Javascript](https://flaviocopes.com/how-to-compare-dates-javascript/)

En utilisant les méthodes et les connaissances présentées dans cet article, vous serez en mesure de comparer efficacement des dates en JavaScript. N'hésitez pas à explorer davantage et à approfondir vos connaissances sur les objets Date afin de les utiliser au mieux dans votre code.