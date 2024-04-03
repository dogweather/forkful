---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:33:37.703402-07:00
description: "Escrever no erro padr\xE3o (stderr) envolve emitir mensagens de erro\
  \ e diagn\xF3sticos no console ou terminal. Os programadores fazem isso para separar\
  \ as\u2026"
lastmod: '2024-03-13T22:44:46.472177-06:00'
model: gpt-4-0125-preview
summary: "Escrever no erro padr\xE3o (stderr) envolve emitir mensagens de erro e diagn\xF3\
  sticos no console ou terminal."
title: "Escrevendo para o erro padr\xE3o"
weight: 25
---

## O Que & Por Que?
Escrever no erro padrão (stderr) envolve emitir mensagens de erro e diagnósticos no console ou terminal. Os programadores fazem isso para separar as informações de erro da saída padrão (stdout), facilitando a depuração e análise de logs.

## Como fazer:

### Saída básica no stderr em Java
Java oferece uma maneira simples de escrever no stderr usando `System.err.print()` ou `System.err.println()`. Veja como fazer isso:

```java
public class ExemploStdErr {
    public static void main(String[] args) {
        try {
            int divisao = 10 / 0;
        } catch (ArithmeticException e) {
            System.err.println("Erro: Não é possível dividir por zero.");
        }
    }
}
```

Saída de exemplo:

```
Erro: Não é possível dividir por zero.
```

Isso imprimirá diretamente a mensagem de erro no stream de erro padrão.

### Usando um Logger para Tratamento Avançado de Erros
Para aplicações que precisam de um tratamento de erros mais sofisticado e logs, usar uma biblioteca de logging como SLF4J com Logback ou Log4J2 é comum. Isso permite mais flexibilidade na gestão da saída de erros, incluindo redirecionamento de arquivos, filtragem e formatação.

#### Exemplo com Logback

Primeiro, adicione a dependência para Logback no seu arquivo `pom.xml` (Maven) ou `build.gradle` (Gradle). Para Maven:

```xml
<dependency>
    <groupId>ch.qos.logback</groupId>
    <artifactId>logback-classic</artifactId>
    <version>1.2.3</version>
</dependency>
```

Então, você pode usar o código seguinte para registrar erros:

```java
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class ExemploLogger {
    private static final Logger logger = LoggerFactory.getLogger(ExemploLogger.class);
    
    public static void main(String[] args) {
        try {
            int resultado = 10 / 0;
        } catch (ArithmeticException e) {
            logger.error("Erro: Não é possível dividir por zero.", e);
        }
    }
}
```

Isso emitirá a mensagem de erro junto com um rastreamento de pilha no console ou em um arquivo, dependendo da configuração do Logback.

Usar frameworks de log como Logback fornece mais controle sobre o tratamento de erros, tornando mais fácil gerenciar aplicativos e sistemas grandes.
