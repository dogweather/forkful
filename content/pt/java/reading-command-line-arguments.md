---
title:    "Java: Lendo argumentos de linha de comando"
keywords: ["Java"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/java/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

# Por que ler argumentos da linha de comando?

Ler argumentos da linha de comando é uma habilidade importante para qualquer programador Java. Ao fazer isso, você permite que seu programa receba entrada do usuário e, portanto, seja mais flexível e interativo. Além disso, entender como trabalhar com os argumentos da linha de comando é uma habilidade fundamental para construir aplicativos mais robustos e eficientes.

## Como fazer

Para ler argumentos da linha de comando em um programa Java, precisamos usar um objeto chamado "String[] args". Isso nos permite acessar os argumentos passados ​​para o programa quando ele é executado. Por exemplo:

```Java
public class MeuPrograma {
    public static void main(String[] args) {
        // Imprime o primeiro argumento inserido pelo usuário na linha de comando
        System.out.println("Argumento 1: " + args[0]);
    }
}
```

Se o usuário digitar "java MeuPrograma argumentoUm argumentoDois", a saída seria:

```
Argumento 1: argumentoUm
```

## Mergulho profundo

Além de acessar os argumentos individualmente, também podemos percorrer todos eles usando um loop for e imprimir todos eles:

```Java
public class MeuPrograma {
    public static void main(String[] args) {
        // Percorre todos os argumentos e imprime cada um
        for (String arg : args) {
            System.out.println(arg);
        }
    }
}
```

Podemos até mesmo verificar o número de argumentos passados ​​pelo usuário e lidar com diferentes cenários em nosso programa:

```Java
public class MeuPrograma {
    public static void main(String[] args) {
        // Verifica o número de argumentos inseridos pelo usuário
        if (args.length == 0) {
            System.out.println("Nenhum argumento inserido.");
        } else if (args.length > 3) {
            System.out.println("Muitos argumentos inseridos.");
        } else {
            // Se houver apenas 1-3 argumentos, imprime cada um separadamente
            for (String arg : args) {
                System.out.println(arg);
            }
        }
    }
}
```

Com esses exemplos e prática, você estará pronto para trabalhar com argumentos da linha de comando em seus programas Java.

# Veja também

- Documentação oficial da Oracle sobre argumentos da linha de comando em Java: https://docs.oracle.com/javase/tutorial/essential/environment/cmdLineArgs.html
- Excelente explicação e exemplos da leitura de argumentos da linha de comando em Java: https://www.baeldung.com/java-command-line-arguments