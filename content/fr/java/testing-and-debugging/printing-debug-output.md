---
title:                "Affichage des sorties de débogage"
aliases:
- /fr/java/printing-debug-output.md
date:                  2024-01-20T17:52:38.818610-07:00
model:                 gpt-4-1106-preview
simple_title:         "Affichage des sorties de débogage"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/java/printing-debug-output.md"
---

{{< edit_this_page >}}

## What & Why? (Quoi et Pourquoi ?)
Le débogage, on connaît : imprimer des sorties dans la console pour comprendre ce qui se passe dans le code. Pourquoi ? Pour traquer les bugs plus vite et vérifier le comportement de notre programme.

## How to (Comment faire) :
En Java, `System.out.println()` est notre ami pour afficher des messages de débogage.

```java
public class DebugExample {
    public static void main(String[] args) {
        int sum = 0;
        for(int i = 1; i <= 5; i++) {
            sum += i;
            System.out.println("Ajout de " + i + ": somme = " + sum); // Affichage pour le débogage
        }
        System.out.println("Somme finale : " + sum);
    }
}
```

Sortie :
```
Ajout de 1: somme = 1
Ajout de 2: somme = 3
Ajout de 3: somme = 6
Ajout de 4: somme = 10
Ajout de 5: somme = 15
Somme finale : 15
```

## Deep Dive (Plongée profonde) :
Historiquement, des méthodes comme `System.out.println()` sont utilisées depuis l'origine de Java. C'est simple et direct. Mais on a mieux maintenant : les loggers (comme Log4j ou SLF4J) permettent de contrôler ce qu'on imprime avec des niveaux (ERROR, INFO, DEBUG...). Ils offrent plus de flexibilité : on peut désactiver les logs de débogage en production ou les diriger vers un fichier.

Implémentation avec un logger :

```java
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class DebugWithLogger {
    private static final Logger logger = LoggerFactory.getLogger(DebugWithLogger.class);
    
    public static void main(String[] args) {
        int sum = 0;
        for(int i = 1; i <= 5; i++) {
            sum += i;
            logger.debug("Ajout de {}: somme = {}", i, sum); // Affichage plus élégant
        }
        logger.info("Somme finale : {}", sum);
    }
}
```

## See Also (Voir aussi) :
- [Documentation officielle de System.out](https://docs.oracle.com/javase/8/docs/api/java/lang/System.html#out)
- [Guide SLF4J](http://www.slf4j.org/manual.html)
- [Le JournalDev sur les loggers en Java](https://www.journaldev.com/977/logger-in-java-logging-example)
