---
title:                "Java: Écrire dans la sortie d'erreur standard."
simple_title:         "Écrire dans la sortie d'erreur standard."
programming_language: "Java"
category:             "Java"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/java/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Pourquoi écrire vers la sortie d'erreur standard ?

Il est important pour les programmeurs Java de comprendre l'importance d'écrire vers la sortie d'erreur standard ou stderr (standard error) lors de la création de leurs programmes. Voici pourquoi.

## Comment faire ?

L'écriture vers la sortie d'erreur standard est très simple en Java. Utilisez simplement la méthode ```System.err.println()``` dans votre code pour écrire un message dans la sortie d'erreur standard. Voici un exemple de code :

```Java
public class Main {
    public static void main(String[] args) {
        System.err.println("Ceci est un message d'erreur !");
    }
}
```

Et voici le résultat que vous obtiendrez dans la sortie d'erreur standard :

```
Ceci est un message d'erreur !
```

Vous pouvez également utiliser la méthode ```System.err.print()``` pour écrire sans retour à la ligne. Vous pouvez également utiliser l'objet ```System.err``` pour écrire directement vers la sortie d'erreur standard comme ceci :

```Java
System.err.append("Ceci est un autre message d'erreur !");
```

## Plongée en profondeur

La sortie d'erreur standard est une fenêtre importante pour les programmeurs Java lorsqu'ils rencontrent des erreurs dans leur code. Lorsqu'une exception est levée, elle est généralement écrite dans la sortie d'erreur standard pour informer le programmeur de l'erreur et de son emplacement dans le code. L'écriture vers la sortie d'erreur standard est également utile pour le débogage de code, car cela peut aider à identifier les erreurs et les problèmes dans le code.

Il est également important de noter que la sortie d'erreur standard est différente de la sortie standard (stdout). La sortie standard est utilisée pour afficher les résultats normaux du programme, tandis que la sortie d'erreur standard est utilisée pour afficher les erreurs et les avertissements.

## Voir aussi

- [Documentation officielle de Java sur System.err](https://docs.oracle.com/javase/7/docs/api/java/lang/System.html#err)
- [Guide de débogage en Java](https://www.baeldung.com/java-debugging)
- [Utilisation de la sortie d'erreur standard en Java](https://www.geeksforgeeks.org/java-io-printstream-println-method-with-examples/)