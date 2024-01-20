---
title:                "Lendo argumentos de linha de comando"
html_title:           "Arduino: Lendo argumentos de linha de comando"
simple_title:         "Lendo argumentos de linha de comando"
programming_language: "Java"
category:             "Java"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/java/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## O Que & Por Quê?

A leitura de argumentos da linha de comando é uma prática que permite que os programadores passem informações ao programa durante a inicialização. Isso fornece mais controle e flexibilidade ao comportamento do seu software.

## Como Fazer:
Segue o exemplo:

```Java
public class ArgumentosCommandLine {
    public static void main(String[] args) {
        for(String arg: args){
            System.out.println("Argumento fornecido = " + arg);
        }
    }
}
```
Correndo o programa com `java ArgumentosCommandLine Oi programador`, o output será:

```
Argumento fornecido = Oi
Argumento fornecido = programador
```

## Mergulho Profundo

Tradicionalmente, ler argumentos da linha de comando em Java é fácil devido ao array de Strings (`String[] args`) passado para a função `main()`. Esta prática é herdada da linguagem C.

Alternativamente, pode-se usar uma biblioteca como a Apache Commons CLI para ter mais opções e controle ao lidar com argumentos complexos.

Os detalhes da implementação são importantes. Embora comandos em lote e scripts de shell geralmente aceitem argumentos sem a necessidade de manipulação adicional significativa, a aplicação de argumentos no caso de programas Java precisa de um cuidado extra. Em Java, argumentos são acessados através do array `args[]` mencionado anteriormente, que é carregado com dados na inicialização do programa.

## Veja Também

1. [Oracle Java Documentation on the Main Method](https://docs.oracle.com/javase/tutorial/essential/environment/cmdLineArgs.html)
2. [Apache Commons CLI Library](https://commons.apache.org/proper/commons-cli/)