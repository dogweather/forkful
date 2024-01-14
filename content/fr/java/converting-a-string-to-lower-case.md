---
title:                "Java: Convertir une chaîne en minuscules"
programming_language: "Java"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/java/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Pourquoi
Les programmeurs utilisent souvent la fonction de conversion en minuscule lorsque leur code nécessite de manipuler ou de comparer des chaînes de caractères. Cela leur permet de s'assurer que les données saisies par l'utilisateur ou récupérées à partir d'une source externe sont uniformisées avant d'être traitées.

## Comment faire
```Java
String str = "JE SUIS EN MINUSCULE";
str = str.toLowerCase();
System.out.println(str);
```
Output : je suis en minuscule

La méthode toLowerCase() peut être appliquée sur un objet de type String et renvoie une nouvelle chaîne de caractères en minuscules.

## Plongez plus profondément
Il est important de noter que cette conversion en minuscule suit les règles de l'encodage par défaut de la plateforme Java sur laquelle le code est exécuté. Cela signifie que si le code est exécuté sur une plateforme avec une police de caractères différente, la conversion peut varier en fonction de l'encodage par défaut.

De plus, la méthode toLowerCase() utilise la locale par défaut de la plateforme pour effectuer la conversion. Cela peut entraîner des différences entre les résultats sur différentes plates-formes qui ont des locales différentes par défaut.

Enfin, cette méthode ne prend en compte que les caractères alphabétiques lors de la conversion en minuscule. Les caractères spéciaux ou non alphabétiques resteront inchangés. Cela peut causer des problèmes si le code utilise des caractères spéciaux, tels que des accents ou des symboles, dans ses chaînes de caractères.

## Voir aussi
- [La documentation officielle de la méthode toLowerCase() en Java](https://docs.oracle.com/javase/8/docs/api/java/lang/String.html#toLowerCase--)
- [Un tutoriel sur les méthodes String en Java](https://www.geeksforgeeks.org/string-methods-java-examples-set-1/)
- [Une discussion sur l'utilisation de la locale dans toLowerCase()](https://stackoverflow.com/questions/36622743/how-is-local-default-determined-when-invoking-string-tolowercase)