---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:34:26.810313-07:00
description: "Comment faire : En Kotlin, \xE9crire sur stderr peut \xEAtre r\xE9alis\xE9\
  \ en utilisant `System.err.println()`. Cette m\xE9thode est similaire \xE0 `System.out.println()`\u2026"
lastmod: '2024-03-13T22:44:57.758086-06:00'
model: gpt-4-0125-preview
summary: "En Kotlin, \xE9crire sur stderr peut \xEAtre r\xE9alis\xE9 en utilisant\
  \ `System.err.println()`."
title: "\xC9crire sur l'erreur standard"
weight: 25
---

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
