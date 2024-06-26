---
date: 2024-01-26 03:45:08.849215-07:00
description: "Comment faire : Java offre plusieurs mani\xE8res d\u2019arrondir les\
  \ nombres. Voici une d\xE9mo rapide avec `Math.round()`, `BigDecimal` et `DecimalFormat`."
lastmod: '2024-03-13T22:44:57.633478-06:00'
model: gpt-4-0125-preview
summary: "Java offre plusieurs mani\xE8res d\u2019arrondir les nombres."
title: Arrondir les nombres
weight: 13
---

## Comment faire :
Java offre plusieurs manières d’arrondir les nombres. Voici une démo rapide avec `Math.round()`, `BigDecimal` et `DecimalFormat`.

```java
public class RoundingDemo {
    public static void main(String[] args) {
        double num = 123.4567;

        // Utilisation de Math.round()
        long roundedNum = Math.round(num);
        System.out.println(roundedNum); // Sortie : 123

        // Utilisation de BigDecimal pour plus de contrôle
        BigDecimal bd = new BigDecimal(num).setScale(2, RoundingMode.HALF_UP);
        double roundedBigDecimal = bd.doubleValue();
        System.out.println(roundedBigDecimal); // Sortie : 123.46

        // Utilisation de DecimalFormat
        DecimalFormat df = new DecimalFormat("#.##");
        String formattedNum = df.format(num);
        System.out.println(formattedNum); // Sortie : 123.46
    }
}
```

## Approfondissement
Historiquement, l’arrondissement des nombres a été essentiel pour les calculs analogiques et s’est transmis à l'informatique numérique pour l'efficacité et la précision. Les erreurs d’arrondi, comme celles issues de l’arithmétique en virgule flottante, démontrent que ce n’est pas un problème trivial – elles peuvent accumuler et perturber les calculs dans, disons, l’aérospatiale et les applications financières.

Au-delà de `Math.round()`, vous avez `BigDecimal`, qui vous donne un contrôle plus fin sur l’échelle et le mode d’arrondissement, et `DecimalFormat` pour quand vous avez besoin d’arrondir les nombres dans le cadre de la mise en forme de la sortie texte. Les alternatives à l'arrondissement incluent l'abaissement, le plafonnement, et la troncature, qui sont différentes manières de gérer la précision et sont généralement gérées par diverses méthodes de `Math`.

Selon votre cas d'utilisation, la stratégie d’arrondissement peut varier. Par exemple, `BigDecimal` est privilégié pour les calculs financiers, où la précision est critique. En contraste, `Math.round()` est une manière rapide pour des opérations à but général où vous êtes moins pointilleux sur le mode d’arrondissement.

## Voir Aussi
- [Documentation Java Math d'Oracle](https://docs.oracle.com/en/java/javase/17/docs/api/java.base/java/lang/Math.html)
- [Norme IEEE pour l’arithmétique à virgule flottante (IEEE 754)](https://ieeexplore.ieee.org/document/4610935)
- [Classe DecimalFormat en Java](https://docs.oracle.com/javase/7/docs/api/java/text/DecimalFormat.html)
