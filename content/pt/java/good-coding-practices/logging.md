---
date: 2024-01-26 01:06:54.382755-07:00
description: "Como fazer: Aqui est\xE1 uma maneira simples de come\xE7ar com log\xED\
  stica em Java usando o pacote integrado `java.util.logging`."
lastmod: '2024-03-13T22:44:46.462410-06:00'
model: gpt-4-1106-preview
summary: "Aqui est\xE1 uma maneira simples de come\xE7ar com log\xEDstica em Java\
  \ usando o pacote integrado `java.util.logging`."
title: Registro de Logs
weight: 17
---

## Como fazer:
Aqui está uma maneira simples de começar com logística em Java usando o pacote integrado `java.util.logging`.

```java
import java.util.logging.Logger;
import java.util.logging.Level;

public class AppLogging {
    private final static Logger LOGGER = Logger.getLogger(Logger.GLOBAL_LOGGER_NAME);

    public static void main(String[] args) {
        LOGGER.info("Registrando uma mensagem no nível INFO");

        try {
            int division = 10 / 0;
        } catch (ArithmeticException e) {
            LOGGER.log(Level.SEVERE, "Ocorreu uma exceção", e);
        }
    }
}
```

Isso produziria uma saída como esta:

```
Jul 03, 2023 2:00:00 PM AppLogging main
INFO: Registrando uma mensagem no nível INFO
Jul 03, 2023 2:00:00 PM AppLogging main
SEVERE: Ocorreu uma exceção
java.lang.ArithmeticException: / por zero
    at AppLogging.main(AppLogging.java:10)
```

## Aprofundando-se
A logística em Java evoluiu bastante. Historicamente, a logística era mais ad-hoc com sistemas de saída e mecanismos escritos pelos próprios usuários. No entanto, a necessidade de padronização levou a APIs de logística como `Log4j` e `SLF4J`. O pacote `java.util.logging` foi introduzido no JDK 1.4, proporcionando uma maneira padronizada de registrar mensagens.

Alternativas ao `java.util.logging` (JUL) incluem Log4j 2 e SLF4J. Enquanto o JUL está integrado ao Java e, portanto, não requer dependências adicionais, tanto o Log4j 2 quanto o SLF4J oferecem recursos mais avançados como controle mais granular sobre a configuração de logística, logística assíncrona e melhor desempenho.

Em termos de implementação, a logística pode ser síncrona, onde cada mensagem de log é processada na thread que a gerou, ou assíncrona, onde as mensagens são entregues a uma thread separada. A logística assíncrona pode melhorar o desempenho, mas introduz complexidade, pois é necessário lidar com a concorrência e garantir que as mensagens de log não sejam perdidas em caso de falha na aplicação.

## Veja também
- [Log4j 2](https://logging.apache.org/log4j/2.x/)
- [SLF4J](http://www.slf4j.org/)
- [Visão geral oficial da logística da Oracle](https://docs.oracle.com/javase/8/docs/technotes/guides/logging/overview.html)
- [Tutorial sobre java.util.logging](https://www.vogella.com/tutorials/Logging/article.html)
