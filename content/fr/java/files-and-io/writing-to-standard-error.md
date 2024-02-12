---
title:                "Écrire sur l'erreur standard"
aliases:
- /fr/java/writing-to-standard-error/
date:                  2024-02-03T19:33:38.923033-07:00
model:                 gpt-4-0125-preview
simple_title:         "Écrire sur l'erreur standard"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/java/writing-to-standard-error.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?
Écrire sur la sortie d'erreur standard (stderr) consiste à afficher des messages d'erreur et des diagnostics sur la console ou le terminal. Les programmeurs le font pour séparer les informations d'erreur de la sortie standard (stdout), facilitant ainsi le débogage et l'analyse des logs.

## Comment faire :

### Sortie stderr basique en Java
Java fournit une manière simple d'écrire sur stderr en utilisant `System.err.print()` ou `System.err.println()`. Voici comment vous le faites :

```java
public class StdErrExample {
    public static void main(String[] args) {
        try {
            int division = 10 / 0;
        } catch (ArithmeticException e) {
            System.err.println("Erreur : Impossible de diviser par zéro.");
        }
    }
}
```

Sortie d'exemple :

```
Erreur : Impossible de diviser par zéro.
```

Cela imprimera directement le message d'erreur sur le flux d'erreur standard.

### Utiliser un système de log pour une gestion d'erreurs avancée
Pour des applications nécessitant une gestion d'erreurs et des logs plus sophistiqués, l'utilisation d'une bibliothèque de journalisation comme SLF4J avec Logback ou Log4J2 est courante. Cela permet une plus grande flexibilité dans la gestion de la sortie d'erreurs, incluant la redirection de fichiers, le filtrage et la mise en forme.

#### Exemple avec Logback

D'abord, ajoutez la dépendance pour Logback à votre fichier `pom.xml` (Maven) ou `build.gradle` (Gradle). Pour Maven :

```xml
<dependency>
    <groupId>ch.qos.logback</groupId>
    <artifactId>logback-classic</artifactId>
    <version>1.2.3</version>
</dependency>
```

Ensuite, vous pouvez utiliser le code suivant pour enregistrer les erreurs :

```java
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class LoggerExample {
    private static final Logger logger = LoggerFactory.getLogger(LoggerExample.class);
    
    public static void main(String[] args) {
        try {
            int result = 10 / 0;
        } catch (ArithmeticException e) {
            logger.error("Erreur : Impossible de diviser par zéro.", e);
        }
    }
}
```

Cela produira le message d'erreur avec une trace de pile sur la console ou dans un fichier, selon la configuration de Logback.

Utiliser des cadres de journalisation comme Logback offre plus de contrôle sur la gestion des erreurs, rendant plus facile la gestion des applications et des systèmes de grande envergure.
