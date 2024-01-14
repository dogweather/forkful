---
title:                "Java: Lendo argumentos da linha de comando."
programming_language: "Java"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/java/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Por que

Se você já se perguntou como é possível passar informações diretamente para um programa Java ao executá-lo, este artigo é para você. Descubra como ler argumentos de linha de comando em um programa Java e como utilizá-los para tornar sua experiência com a linha de comando ainda mais eficiente.

## Como Fazer

Para ler argumentos de linha de comando em um programa Java, é necessário utilizar a classe "```JavaScanner```" e o método "```Javaargs```". Abaixo está um exemplo do código que pode ser utilizado:

```Java
import java.util.Scanner; 

public class LeitorArgs {
    public static void main (String[] args) {
        Scanner sc = new Scanner(System.in);
        System.out.println("Digite o seu nome:");
        String nome = sc.nextLine();

        System.out.println("Olá, " + nome + "! Seja bem-vindo!");
    }
}
```

Ao executar este código no terminal, é possível passar um argumento após o nome do programa, como por exemplo: "```Java LeitorArgs Maria```". O programa vai ler esse argumento e imprimir "Olá, Maria! Seja bem-vindo!" no terminal. Se nenhum argumento for passado, o programa irá solicitar ao usuário que digite seu nome.

## Aprofundando

Ler argumentos de linha de comando pode ser útil em muitas situações, como por exemplo, quando se está automatizando processos através de scripts ou quando se deseja fornecer valores a um programa sem a necessidade de interagir com uma interface gráfica. Além disso, essa habilidade pode ser especialmente útil quando se está trabalhando com ferramentas de linha de comando, como o Git.

É importante ressaltar que os argumentos de linha de comando são passados para o programa como uma lista de strings. Portanto, é necessário converter esses valores para o tipo de dado desejado, caso sejam necessários.

## Veja Também

- Documentação oficial da classe Scanner: [https://docs.oracle.com/javase/8/docs/api/java/util/Scanner.html](https://docs.oracle.com/javase/8/docs/api/java/util/Scanner.html)
- Documentação oficial do método args: [https://docs.oracle.com/javase/8/docs/api/java/lang/String.html#args](https://docs.oracle.com/javase/8/docs/api/java/lang/String.html#args)
- Tutorial sobre como ler argumentos de linha de comando em Java: [https://www.tutorialspoint.com/java/java_command_line_arguments.htm](https://www.tutorialspoint.com/java/java_command_line_arguments.htm)