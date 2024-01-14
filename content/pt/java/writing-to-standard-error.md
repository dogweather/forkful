---
title:                "Java: Escrevendo para o erro padrão"
programming_language: "Java"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/java/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Por que escrever para o erro padrão?

Escrever para o erro padrão é uma prática comum na programação Java. Ela permite que os desenvolvedores possam visualizar mensagens de erro e depuração do código durante a execução do programa.

## Como fazer isso:

Existem duas maneiras de escrever para o erro padrão em Java: usando a classe System e usando a classe Logger. Veja abaixo exemplos de código em Java e suas respectivas saídas.

#### Usando a classe System:

```Java
System.err.println("Ocorreu um erro!");
```

Saída:
```
Ocorreu um erro!
```

#### Usando a classe Logger:

```Java
Logger logger = Logger.getLogger("MeuPrograma");
logger.warning("Aviso: a conexão com o banco de dados falhou!");
```

Saída:
```
06-Mai-2021 12:00:00 WARNING MeuPrograma: Aviso: a conexão com o banco de dados falhou!
```

## Mergulho profundo:

Escrever para o erro padrão com a classe System permite que o desenvolvedor exiba mensagens de erro e depuração diretamente no console. Já a utilização da classe Logger oferece mais recursos, como a possibilidade de definir o nível de severidade das mensagens e o destino das mesmas (console, arquivo de log, entre outros).

Existem diferentes níveis de severidade em um Logger, sendo os principais: info, warning e severe. É possível definir o nível desejado para cada mensagem, o que facilita a identificação e filtragem das mesmas.

## Veja também:

- Documentação oficial do Java sobre a classe System: https://docs.oracle.com/javase/tutorial/essential/io/sysout.html
- Documentação oficial do Java sobre a classe Logger: https://docs.oracle.com/javase/8/docs/api/java/util/logging/Logger.html
- Tutorial sobre como usar a classe Logger em Java: https://www.baeldung.com/java-logging-intro