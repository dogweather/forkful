---
date: 2024-01-26 04:41:49.084601-07:00
description: "Les nombres complexes \xE9tendent la ligne des nombres r\xE9els par\
  \ l'ajout d'une unit\xE9 imaginaire, `i`, o\xF9 `i^2 = -1`. Ils sont cruciaux dans\
  \ des domaines\u2026"
lastmod: 2024-02-19 22:05:16.394393
model: gpt-4-0125-preview
summary: "Les nombres complexes \xE9tendent la ligne des nombres r\xE9els par l'ajout\
  \ d'une unit\xE9 imaginaire, `i`, o\xF9 `i^2 = -1`. Ils sont cruciaux dans des domaines\u2026"
title: Manipulation des nombres complexes
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?

Les nombres complexes étendent la ligne des nombres réels par l'ajout d'une unité imaginaire, `i`, où `i^2 = -1`. Ils sont cruciaux dans des domaines comme l'ingénierie, la physique et les mathématiques avancées, où ils modélisent des phénomènes que les nombres réels ne peuvent pas gérer, comme les courants électriques et le traitement des signaux.

## Comment faire :

Java n'a pas de support intégré pour les nombres complexes, mais nous pouvons créer notre propre classe ou utiliser une bibliothèque. Voici un exemple rapide de comment créer une classe `ComplexNumber` simple et l'utiliser :

```java
public class ComplexNumber {
    private double reel;
    private double imaginaire;

    public ComplexNumber(double reel, double imaginaire) {
        this.reel = reel;
        this.imaginaire = imaginaire;
    }

    public ComplexNumber add(ComplexNumber autre) {
        return new ComplexNumber(this.reel + autre.reel, this.imaginaire + autre.imaginaire);
    }

    // ToString pour afficher les nombres complexes sous forme a + bi
    @Override
    public String toString() {
        return String.format("%.1f + %.1fi", reel, imaginaire);
    }

    // Test rapide
    public static void main(String[] args) {
        ComplexNumber c1 = new ComplexNumber(2, 3);
        ComplexNumber c2 = new ComplexNumber(1, 4);

        System.out.println("Somme : " + c1.add(c2));
    }
}
```

Le résultat d'exécution de la méthode main sera :

```
Somme : 3.0 + 7.0i
```

## Approfondissement

Avant les langages de haut niveau comme Java, les programmeurs travaillaient directement avec des bibliothèques mathématiques dans des langages comme Fortran ou C pour gérer des opérations complexes. Le concept remonte au 16e siècle, crédité à des mathématiciens comme Gerolamo Cardano et Rafael Bombelli.

Dans Java, `java.lang.Math` est la référence pour les essentiels mais évite les nombres complexes, probablement parce que tous les programmeurs ne les utilisent pas. Des alternatives ? Utilisez des bibliothèques. Apache Commons Math fournit une classe `Complex` pleine de méthodes pour la manipulation. Voici pourquoi créer la vôtre est intéressant : C'est léger, adapté à vos besoins exacts, et sans surcharge de bibliothèque.

Un détail important : faites attention à la précision en virgule flottante. Les ordinateurs ne peuvent pas représenter certains nombres exactement, conduisant à des erreurs d'arrondi. Lors de l'exécution d'opérations complexes répétitives, ces erreurs peuvent s'accumuler !

## Voir Aussi

Pour des études plus approfondies et des opérations plus complexes, consultez :

- [Apache Commons Math](https://commons.apache.org/proper/commons-math/)
- [La classe Complex de JScience](http://jscience.org/)
- Les tutoriels d'Oracle sur [l'arithmétique à virgule flottante](https://docs.oracle.com/cd/E19957-01/806-3568/ncg_goldberg.html)
