---
title:    "Java: Comparaison de deux dates"
keywords: ["Java"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/java/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Pourquoi 

Comparer deux dates est une tâche courante dans la programmation Java. Cela peut être utile pour vérifier si une date est antérieure, ultérieure ou égale à une autre, ou pour calculer la durée écoulée entre deux dates. Dans ce blog, nous allons explorer comment comparer deux dates en Java et comment utiliser cette fonctionnalité dans vos projets.

## Comment faire 

Pour comparer deux dates en Java, vous pouvez utiliser la classe `LocalDate` du package `java.time`. Cette classe représente une date sans heure et peut être instanciée de différentes manières. Dans l'exemple suivant, nous allons créer deux objets `LocalDate` et les comparer pour vérifier si la première date est antérieure à la deuxième :

```Java 
LocalDate date1 = LocalDate.of(2021, 1, 1);
LocalDate date2 = LocalDate.of(2021, 2, 1);

// comparaison
if (date1.isBefore(date2)) {
    System.out.println("Date 1 est antérieure à la Date 2");
} 
```

Dans cet exemple, nous créons deux objets `LocalDate`, un avec la date du 1er janvier 2021 et l'autre avec la date du 1er février 2021. Nous utilisons ensuite la méthode `isBefore()` pour vérifier si la date 1 est antérieure à la date 2. Si c'est le cas, nous affichons un message à l'écran.

La classe `LocalDate` a également d'autres méthodes utiles pour comparer deux dates, telles que `isAfter()` pour vérifier si une date est ultérieure à une autre, ou `isEqual()` pour vérifier si elles sont égales.

## Plongée en profondeur 

En Java, les dates sont stockées sous forme de nombres, ce qui facilite leur comparaison. Cependant, il est important de comprendre le fonctionnement de ces nombres pour éviter les erreurs de comparaison de dates. Par exemple, le 1er janvier 2021 sera représenté par le nombre 20210101, tandis que le 1er février 2021 sera représenté par le nombre 20210201. Cela signifie que si vous comparez deux dates en utilisant simplement l'opérateur de comparaison `>`, vous risquez d'obtenir de faux résultats car le 1er février sera considéré comme étant antérieur au 1er janvier, en raison de la valeur numérique inférieure.

## Voir aussi
- La documentation officielle de la classe LocalDate de Java : https://docs.oracle.com/javase/8/docs/api/java/time/LocalDate.html
- Un tutoriel sur les dates et le temps en Java : https://www.informit.com/articles/article.aspx?p=2447709&seqNum=3