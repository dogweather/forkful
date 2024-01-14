---
title:    "C: Lendo argumentos de linha de comando"
keywords: ["C"]
---

{{< edit_this_page >}}

##Por que

Se você é um programador iniciante ou já tem alguma experiência em C, é provável que já tenha ouvido falar sobre argumentos de linha de comando. Eles podem parecer intimidadores no começo, mas são uma ferramenta poderosa e essencial na programação. Neste post, vamos discutir por que é importante entender como ler argumentos de linha de comando e como fazê-lo em C.

##Como Fazer

Ler argumentos de linha de comando é uma maneira eficiente de fornecer entradas para um programa sem a necessidade de recompilar o código toda vez que deseja alterá-las. Para isso, utilizamos a função `main()` e os parâmetros `argc` e `argv`. 

O `argc` é um inteiro que indica o número de argumentos passados na linha de comando, enquanto o `argv` é um vetor de strings que contém os próprios argumentos. Vamos ver um exemplo de código:

```
#include <stdio.h>

int main(int argc, char *argv[]) {
  printf("O número de argumentos é: %d \n", argc);
  printf("O primeiro argumento é: %s\n", argv[0]);
  
  return 0;
}
```
Neste exemplo, utilizamos a função `printf()` para exibir o número de argumentos e o primeiro argumento passado na linha de comando. Note que `argv` é um vetor, então o primeiro elemento é acessado através de `argv[0]`.

Agora, vamos compilar e executar o nosso programa com alguns argumentos na linha de comando:

```
$ gcc arg.c -o arg
$ ./arg arg1 arg2 arg3
```

A saída será:
```
O número de argumentos é: 4
O primeiro argumento é: ./arg
```
Note que o primeiro argumento é sempre o nome do programa em si.

Podemos utilizar um loop para acessar todos os argumentos passados na linha de comando:

```
#include <stdio.h>

int main(int argc, char *argv[]) {
  for (int i = 1; i < argc; i++) {
    printf("Argumento %d: %s\n", i, argv[i]);
  }
  
  return 0;
}
```

Agora, se executarmos o programa com mais argumentos:

```
$ ./arg arg1 arg2 arg3
```

A saída será:
```
Argumento 1: arg1
Argumento 2: arg2
Argumento 3: arg3
```

##Aprofundamento

Além de ler argumentos simples, é possível também passar argumentos com opções na linha de comando. Por exemplo, utilizando a opção `-a` para ativar uma funcionalidade específica do programa. Para isso, podemos utilizar a biblioteca padrão `getopt.h`.

Outra funcionalidade interessante é a possibilidade de passar argumentos com valores. Por exemplo, na linha de comando podemos escrever `./programa -n 10`, indicando que o valor de `-n` é 10. Para isso, podemos utilizar a função `atoi()` para converter strings em inteiros.

É importante lembrar que os argumentos passados na linha de comando são sempre lidos como strings, então cabe ao programador realizar as conversões necessárias de acordo com o tipo de dado desejado.

##Veja Também

- [Documentação oficial do C](https://devdocs.io/c/)
- [Tutorial sobre argumentos de linha de comando em C](https://www.geeksforgeeks.org/command-line-arguments-in-c-cpp/)
- [Exemplos de códigos com argumentos de linha de comando](http://zona-utopia.blogspot.com/2009/01/como-passar-argumentos-de-linha-de.html)