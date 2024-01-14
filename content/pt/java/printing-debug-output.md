---
title:    "Java: Imprimindo saída de depuração"
keywords: ["Java"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/java/printing-debug-output.md"
---

{{< edit_this_page >}}

## Por que
Imprimir saída de debug é uma prática fundamental para qualquer programador no processo de depuração de código. Ela permite visualizar dados, variáveis e informações importantes que podem ser úteis para identificar e corrigir erros.

## Como Fazer
Para imprimir saída de debug em Java, é muito simples. O primeiro passo é importar o pacote `java.util.logging` no seu código:

```Java
import java.util.logging.*;
```

Em seguida, é preciso criar um objeto do tipo `Logger` e definir o nível de logging desejado, por exemplo:

```Java
Logger logger = Logger.getLogger("NomeDoArquivo.class");
logger.setLevel(Level.INFO);
```

Agora, podemos utilizar os métodos `info()`, `warning()` e `severe()` para imprimir as mensagens de debug, seguidos pelo `log()` do objeto `Logger`, como no exemplo abaixo:

```Java
logger.info("Mensagem de debug");
logger.log(Level.WARNING, "Aviso: variável x é igual a " + x);
```

Ao executar o código, as mensagens de debug serão impressas no console.

## Deep Dive
Além das informações básicas, é possível aprofundar ainda mais o processo de saída de debug em Java. É possível definir um `Formatter` para personalizar o formato das mensagens de log, como por exemplo:

```Java
formatter = new SimpleFormatter();
consoleLogger.setFormatter(formatter);
```

Também é possível especificar um `Handler` para redirecionar as mensagens de log para diferentes locais, como um arquivo de log ou um banco de dados.

Outra opção é utilizar o método `setUseParentHandlers(false)` para impedir que as mensagens de debug sejam enviadas para cima na hierarquia de loggers, evitando logs duplicados.

## Veja Também
- [Documentação oficial do java.util.logging](https://docs.oracle.com/javase/8/docs/api/java/util/logging/package-summary.html)
- [Tutorial de saída de debug em Java](https://www.baeldung.com/java-logging-intro)
- [Artigo sobre as melhores práticas de logging em Java](https://www.journaldev.com/977/logging-best-practices-java)<Paste>