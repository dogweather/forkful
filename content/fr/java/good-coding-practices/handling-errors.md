---
date: 2024-01-26 00:53:12.922871-07:00
description: "Comment faire : Java utilise des exceptions pour g\xE9rer les erreurs.\
  \ Vous entourez le code risqu\xE9 avec un bloc `try` et attrapez les exceptions\
  \ avec\u2026"
lastmod: '2024-03-13T22:44:57.648612-06:00'
model: gpt-4-1106-preview
summary: "Java utilise des exceptions pour g\xE9rer les erreurs."
title: Gestion des erreurs
weight: 16
---

## Comment faire :
Java utilise des exceptions pour gérer les erreurs. Vous entourez le code risqué avec un bloc `try` et attrapez les exceptions avec `catch`. Voici un exemple simple :

```java
public class ExempleGestionErreur {
    public static void main(String[] args) {
        try {
            int resultat = diviser(10, 0);
            System.out.println("Le résultat est : " + resultat);
        } catch (ArithmeticException e) {
            System.out.println("Oups, on ne peut pas diviser par zéro !");
        }
    }

    private static int diviser(int numerateur, int denominateur) {
        return numerateur / denominateur;
    }
}
```

Sortie :
```
Oups, on ne peut pas diviser par zéro !
```

## Approfondissement
La gestion des erreurs en Java a évolué. Aux premiers jours, il n'y avait pas d'exceptions ; les programmeurs vérifiaient les codes d'erreur. Puis Java a introduit les blocs try-catch, permettant une gestion des erreurs plus élégante.

Des alternatives au `try-catch` traditionnel incluent `try-with-resources` pour la fermeture automatique des ressources et un code plus propre, introduit dans Java 7.

Les détails de mise en œuvre sont importants. Par exemple, attraper `Exception` ou `Throwable` est généralement une mauvaise pratique. C'est trop général et cela peut masquer des bugs dont vous pourriez ne pas être conscient. Tenez-vous-en aux exceptions spécifiques.

## Voir aussi
- Les tutoriels officiels d'Oracle sur les exceptions Java : [https://docs.oracle.com/javase/tutorial/essential/exceptions/](https://docs.oracle.com/javase/tutorial/essential/exceptions/)
- La documentation sur l'instruction `try-with-resources` de Java : [https://docs.oracle.com/javase/tutorial/essential/exceptions/tryResourceClose.html](https://docs.oracle.com/javase/tutorial/essential/exceptions/tryResourceClose.html)
- Effective Java de Joshua Bloch, pour les meilleures pratiques sur les exceptions.
