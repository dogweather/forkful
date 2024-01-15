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

## Por Que

Se você já se perguntou como programas em Java conseguem receber informações dos usuários antes mesmo de serem executados, é porque eles estão lendo os argumentos da linha de comando. Esta é uma habilidade fundamental para qualquer programador em Java, pois permite a interação com o usuário e a personalização da execução do programa.

## Como Fazer

Para ler os argumentos da linha de comando em Java, é necessário utilizar o método `main()` e o parâmetro `args[]`. Veja abaixo um exemplo de código que imprime os argumentos fornecidos pelo usuário:

```Java
public class LeituraArgumentos {
    public static void main(String[] args){
        for (int i = 0; i < args.length; i++){
            System.out.printf("Argumento %d: %s\n", i+1, args[i]);
        }
    }
}
```

Ao executar este programa com o comando `java LeituraArgumentos argumento1 argumento2`, ele irá imprimir o seguinte resultado:

```
Argumento 1: argumento1
Argumento 2: argumento2
```

Também é possível acessar argumentos específicos através da posição no array `args[]`. Por exemplo, `args[0]` irá retornar o primeiro argumento fornecido pelo usuário.

## Mergulho Profundo

Além de simplesmente imprimir os argumentos, também é possível realizar outras operações com eles, como converter para outros tipos de dados ou validar sua entrada. É importante ressaltar que os argumentos são sempre passados como strings, e cabe ao programador realizar as conversões necessárias.

Também é possível acessar a lista de argumentos diretamente fora do método `main()`, basta utilizar a classe `java.lang.System` e seu método `getProperties()`. Este método irá retornar um objeto da classe `java.util.Properties`, que possui um método `getProperty()` que permite a busca de um argumento específico pelo seu nome.

## Veja Também

- [Documentação do método main() em Java](https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/lang/Class.html#main(java.lang.String...))
- [Como passar argumentos na linha de comando em Java](http://www.aprendendo-java.com/2016/09/como-passar-parametros-na-linha-de-comando-em-java.html)
- [Tutorial de leitura de argumentos da linha de comando em Java](https://www.tutorialspoint.com/java/java_command_line_arguments.htm)