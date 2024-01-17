---
title:                "Lendo argumentos da linha de comando"
html_title:           "Java: Lendo argumentos da linha de comando"
simple_title:         "Lendo argumentos da linha de comando"
programming_language: "Java"
category:             "Java"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/java/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## O que e por que?

Ler argumentos de linha de comando é uma técnica usada pelos programadores para permitir que um programa receba informações diretamente da linha de comando do terminal. Isso pode facilitar a execução de um programa com diferentes argumentos e configurações, sem a necessidade de recompilar o código.

## Como fazer:

```
public static void main(String[] args) {
    System.out.println("Ola " + args[0] + "!");
}
```

Executando o código acima com o seguinte comando: ```java MeuPrograma Alice```, veremos o resultado: ```Ola Alice!```

## Mergulho profundo:

Ler argumentos de linha de comando é uma técnica amplamente usada em linguagens de programação, como Java, C++ e Python. Ela se tornou popular com o avanço da automação de tarefas e dos sistemas operacionais baseados em linhas de comando. Existem várias alternativas para ler argumentos de linha de comando, como a biblioteca "getopt" em C++ e a classe "OptionParser" em Python. No entanto, em Java, essa funcionalidade é fornecida pela classe "String[] args" no método main.

## Veja também:

- [Documentação oficial da classe String[] args](https://docs.oracle.com/javase/tutorial/essential/environment/cmdLineArgs.html)
- [Tutorial sobre como usar argumentos de linha de comando em Java](https://www.baeldung.com/java-command-line-arguments)
- [Exemplos práticos de uso de argumentos de linha de comando em Java](https://www.javatpoint.com/command-line-argument-in-java)