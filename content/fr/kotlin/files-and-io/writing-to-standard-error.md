---
title:                "Écrire sur l'erreur standard"
aliases: - /fr/kotlin/writing-to-standard-error.md
date:                  2024-02-03T19:34:26.810313-07:00
model:                 gpt-4-0125-preview
simple_title:         "Écrire sur l'erreur standard"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/kotlin/writing-to-standard-error.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?

Écrire sur l'erreur standard (stderr) consiste à envoyer des messages d'erreur et des diagnostics sur un flux séparé, distinct de la sortie standard (stdout), ce qui permet une meilleure gestion des erreurs et l'analyse des journaux. Les programmeurs font cela pour faciliter le débogage et pour s'assurer que les messages d'erreur peuvent être facilement identifiés et redirigés si nécessaire, en maintenant des journaux de sortie propres ou des messages aux utilisateurs.

## Comment faire :

En Kotlin, écrire sur stderr peut être réalisé en utilisant `System.err.println()`. Cette méthode est similaire à `System.out.println()` mais dirige la sortie vers le flux d'erreur standard plutôt que le flux de sortie standard.

```kotlin
fun main() {
    System.err.println("Ceci est un message d'erreur !")
}
```

Exemple de sortie :
```
Ceci est un message d'erreur !
```

Pour des applications plus structurées ou complexes, particulièrement celles qui impliquent des cadres de journalisation comme Logback ou SLF4J, vous pouvez configurer des enregistreurs pour écrire sur stderr pour certains niveaux de log (par exemple, ERROR).

Utilisation de SLF4J avec Logback :

1. D'abord, ajoutez l'API SLF4J et l'implémentation Logback à votre `build.gradle` :

```groovy
dependencies {
    implementation 'org.slf4j:slf4j-api:1.7.30'
    implementation 'ch.qos.logback:logback-classic:1.2.3'
}
```

2. Ensuite, configurez Logback (dans `src/main/resources/logback.xml`) pour diriger les messages de niveau d'erreur vers stderr :

```xml
<configuration>
    <appender name="STDERR" class="ch.qos.logback.core.ConsoleAppender">
        <target>System.err</target>
        <encoder>
            <pattern>%d{yyyy-MM-dd HH:mm:ss} [%thread] %-5level %logger{36} - %msg%n</pattern>
        </encoder>
    </appender>
    
    <root level="error">
        <appender-ref ref="STDERR" />
    </root>
</configuration>
```

3. Ensuite, utilisez SLF4J dans votre code Kotlin pour enregistrer les messages d'erreur :

```kotlin
import org.slf4j.LoggerFactory

fun main() {
    val logger = LoggerFactory.getLogger("ExampleLogger")
    logger.error("Ceci est un message de journalisation d'erreur !")
}
```

Exemple de sortie (vers stderr) :
```
2023-04-01 12:34:56 [main] ERROR ExampleLogger - Ceci est un message de journalisation d'erreur !
```
