---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:33:48.311137-07:00
description: "Escrever no erro padr\xE3o (stderr) trata de enviar mensagens de erro\
  \ e diagn\xF3sticos para um fluxo separado, distinto da sa\xEDda padr\xE3o (stdout),\
  \ o que permite\u2026"
lastmod: '2024-03-13T22:44:46.558378-06:00'
model: gpt-4-0125-preview
summary: "Escrever no erro padr\xE3o (stderr) trata de enviar mensagens de erro e\
  \ diagn\xF3sticos para um fluxo separado, distinto da sa\xEDda padr\xE3o (stdout),\
  \ o que permite um melhor manejo de erros e an\xE1lise de logs."
title: "Escrevendo para o erro padr\xE3o"
weight: 25
---

## Como fazer:
Em Kotlin, escrever no stderr pode ser alcançado usando `System.err.println()`. Este método é similar a `System.out.println()`, mas direciona a saída para o fluxo de erro padrão em vez do fluxo de saída padrão.

```kotlin
fun main() {
    System.err.println("Esta é uma mensagem de erro!")
}
```

Saída de exemplo:
```
Esta é uma mensagem de erro!
```

Para aplicações mais estruturadas ou complexas, particularmente aquelas que envolvem frameworks de log como Logback ou SLF4J, você pode configurar loggers para escrever no stderr para certos níveis de log (por exemplo, ERROR).

Usando SLF4J com Logback:

1. Primeiro, adicione a API SLF4J e a implementação Logback ao seu `build.gradle`:

```groovy
dependencies {
    implementation 'org.slf4j:slf4j-api:1.7.30'
    implementation 'ch.qos.logback:logback-classic:1.2.3'
}
```

2. Em seguida, configure o Logback (em `src/main/resources/logback.xml`) para direcionar mensagens de nível de erro para stderr:

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

3. Então, use SLF4J no seu código Kotlin para registrar mensagens de erro:

```kotlin
import org.slf4j.LoggerFactory

fun main() {
    val logger = LoggerFactory.getLogger("ExampleLogger")
    logger.error("Esta é uma mensagem de log de erro!")
}
```

Saída de exemplo (para stderr):
```
2023-04-01 12:34:56 [main] ERROR ExampleLogger - Esta é uma mensagem de log de erro!
```
