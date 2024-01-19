---
title:                "Imprimer la sortie de débogage"
html_title:           "Arduino: Imprimer la sortie de débogage"
simple_title:         "Imprimer la sortie de débogage"
programming_language: "Java"
category:             "Java"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/java/printing-debug-output.md"
---

{{< edit_this_page >}}

## Qu'est-ce que c'est & Pourquoi ?

L'impression de la sortie de débogage est une technique aidant les développeurs à comprendre ce qui se passe dans le code pendant son exécution. C'est une manière rapide et simple de suivre l'évolution des variables et du flux de contrôle.

## Comment faire :

Voici un exemple simple de comment imprimer une sortie de débogage en Java. 

```Java
public class DebugExemple {
   public static void main( String args[] ) {
      int a = 5;
      int b = 10;
      System.out.println("La valeur de a est : " + a);
      System.out.println("La valeur de b est : " + b);
      int somme = a + b;
      System.out.println("La somme de a et b est : " + somme);
   }
}
```

Lorsque vous exécutez ce programme, vous obtenez la sortie suivante :

```
La valeur de a est : 5
La valeur de b est : 10
La somme de a et b est : 15
```
Cela nous fournit énormément d'informations sur le fonctionnement interne de notre programme.

## Plongée en profondeur 

Historiquement, l'impression de débogage a été l'une des premières techniques utilisées par les développeurs pour comprendre les erreurs de leurs programmes. Même aujourd'hui, c'est souvent le premier outil que les développeurs utilisent lorsqu'ils rencontrent un bug.

Une alternative moderne à l'impression de débogage est l'utilisation d'un débogueur. Les débogueurs offrent plus de contrôle, car ils peuvent arrêter l'exécution d'un programme à tout moment, permettant aux développeurs d'examiner l'état du programme.

En Java, l'impression de débogage est généralement effectuée en utilisant la méthode `System.out.println()`. Cette méthode imprime la sortie à la console par défaut.

## Voir aussi

Pour en savoir plus sur le débogage en Java, consultez ces liens utiles :

1. Oracle Official Documentation : [Débogage des applications Java](https://docs.oracle.com/javase/7/docs/technotes/guides/debug/index.html)

2. Tutorialspoint : [Débogage Java](https://www.tutorialspoint.com/java/java_debugging.htm)

3. Baeldung : [Un guide pour le débogueur Java](https://www.baeldung.com/java-debugging)