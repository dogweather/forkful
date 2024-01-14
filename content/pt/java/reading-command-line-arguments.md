---
title:    "Java: Lendo argumentos da linha de comando"
keywords: ["Java"]
---

{{< edit_this_page >}}

## Por que ler argumentos da linha de comando?

Entender como ler e utilizar argumentos da linha de comando é essencial para criar programas eficientes e versáteis em Java. Esses argumentos permitem que o usuário insira informações importantes ao executar o programa, tornando-o mais flexível e adaptável às necessidades individuais. Além disso, é uma habilidade fundamental para qualquer programador que deseja trabalhar com aplicativos de linha de comando ou scripts.

## Como fazer

Para ler os argumentos da linha de comando em Java, primeiro é preciso importar a classe `java.util.Scanner` no início do programa. Em seguida, criamos um objeto `Scanner` para ler os argumentos inseridos pelo usuário. Podemos acessar esses argumentos através do método `args[]` da classe `main`. Veja um exemplo abaixo:

```Java
import java.util.Scanner;

public class Arguments {

  public static void main(String[] args) {

    // Cria o objeto Scanner para ler os argumentos
    Scanner scanner = new Scanner(System.in);

    // Imprime o primeiro argumento fornecido pelo usuário
    System.out.println("O primeiro argumento é: " + args[0]);

    // Fecha o objeto Scanner
    scanner.close();
  }
}
```

Ao executar esse programa com o seguinte comando: `java Arguments argumento1 argumento2`, o primeiro argumento inserido será impresso na tela, resultando na saída: `O primeiro argumento é: argumento1`.

Também é importante lembrar que os argumentos da linha de comando são sempre lidos como strings, portanto, se precisarmos de um tipo de dado diferente, devemos convertê-lo manualmente.

## Mergulho profundo

Ao utilizar argumentos da linha de comando, é necessário ter em mente que eles são separados por espaços e podem ser inseridos na ordem que o usuário preferir. Além disso, é possível utilizar aspas para agrupar um argumento que contenha espaços ou caracteres especiais.

Outra dica é utilizar o método `hasNext()` do objeto `Scanner` para verificar se ainda existem argumentos a serem lidos. Isso é especialmente útil quando os argumentos possuem um número variável, permitindo que o programa os leia de forma dinâmica.

## Veja também

- [Documentação oficial da classe `Scanner`](https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/util/Scanner.html)
- [Exemplos de leitura de argumentos da linha de comando em Java](https://www.tutorialspoint.com/java/java_command_line_arguments)
- [Tutorial sobre argumentos da linha de comando em Java](https://www.baeldung.com/java-command-line-arguments)