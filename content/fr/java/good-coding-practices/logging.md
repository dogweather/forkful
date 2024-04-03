---
date: 2024-01-26 01:06:58.361329-07:00
description: "Comment faire : Voici une fa\xE7on simple de commencer \xE0 utiliser\
  \ la journalisation en Java en utilisant le package int\xE9gr\xE9 `java.util.logging`."
lastmod: '2024-03-13T22:44:57.647463-06:00'
model: gpt-4-1106-preview
summary: "Voici une fa\xE7on simple de commencer \xE0 utiliser la journalisation en\
  \ Java en utilisant le package int\xE9gr\xE9 `java.util.logging`."
title: Journalisation
weight: 17
---

## Comment faire :
Voici une façon simple de commencer à utiliser la journalisation en Java en utilisant le package intégré `java.util.logging`.

```java
import java.util.logging.Logger;
import java.util.logging.Level;

public class AppLogging {
    private final static Logger LOGGER = Logger.getLogger(Logger.GLOBAL_LOGGER_NAME);

    public static void main(String[] args) {
        LOGGER.info("Enregistrement d'un message de niveau INFO");

        try {
            int division = 10 / 0;
        } catch (ArithmeticException e) {
            LOGGER.log(Level.SEVERE, "Une exception s'est produite", e);
        }
    }
}
```

Cela produirait une sortie du genre :

```
Jul 03, 2023 2:00:00 PM AppLogging main
INFO: Enregistrement d'un message de niveau INFO
Jul 03, 2023 2:00:00 PM AppLogging main
SEVERE: Une exception s'est produite
java.lang.ArithmeticException: / by zero
    at AppLogging.main(AppLogging.java:10)
```

## Approfondissement
La journalisation en Java a beaucoup évolué. Historiquement, la journalisation était plus ad-hoc avec des sorties système et des mécanismes écrits par soi-même. Cependant, le besoin de standardisation a conduit à des API de journalisation comme `Log4j` et `SLF4J`. Le package `java.util.logging` a lui-même été introduit dans le JDK 1.4, fournissant un moyen standardisé d’enregistrer des messages.

Les alternatives à `java.util.logging` (JUL) comprennent Log4j 2 et SLF4J. Alors que JUL est intégré à Java et donc ne nécessite pas de dépendances supplémentaires, Log4j 2 et SLF4J offrent des fonctionnalités plus avancées comme un contrôle plus granulaire de la configuration de journalisation, une journalisation asynchrone et de meilleures performances.

En termes d'implémentation, la journalisation peut être soit synchrone, où chaque message de journal est traité dans le thread qui l'a généré, soit asynchrone, où les messages sont remis à un thread séparé. La journalisation asynchrone peut améliorer les performances mais introduit de la complexité car il faut gérer la concurrence et s'assurer que les messages de journal ne sont pas perdus en cas de crash de l'application.

## Voir aussi
- [Log4j 2](https://logging.apache.org/log4j/2.x/)
- [SLF4J](http://www.slf4j.org/)
- [Vue d'ensemble officielle de la journalisation par Oracle](https://docs.oracle.com/javase/8/docs/technotes/guides/logging/overview.html)
- [Tutoriel sur java.util.logging](https://www.vogella.com/tutorials/Logging/article.html)
