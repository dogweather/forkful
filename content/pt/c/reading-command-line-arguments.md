---
title:                "C: Lendo argumentos da linha de comando"
simple_title:         "Lendo argumentos da linha de comando"
programming_language: "C"
category:             "C"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/c/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Por que ler argumentos de linha de comando em C?

Se você é um programador iniciante em C ou se já tem algum conhecimento da linguagem, deve ter ouvido falar sobre a importância de ler argumentos de linha de comando. Mas por que exatamente isso é importante? Continue lendo este artigo para descobrir.

## Como fazer isso em C?

Ao trabalhar com linguagens de programação, é comum querer que o programa seja flexível e tenha a capacidade de se adaptar a diferentes situações. Ao permitir que o usuário passe argumentos através da linha de comando, o programa se torna mais versátil e pode ser executado de maneiras diferentes dependendo das necessidades do usuário.

Felizmente, a linguagem C possui uma ferramenta muito útil para ler argumentos de linha de comando: a função `main()`. Esta função é responsável por receber os argumentos e executar o programa de acordo com eles.

Para isso, é preciso definir dois parâmetros para a função `main()`: `argc` e `argv`. O primeiro indica a quantidade de argumentos passados e o segundo é um vetor de strings que contém os próprios argumentos. Veja abaixo um exemplo de como isso pode ser feito:

```C
#include <stdio.h>

int main(int argc, char *argv[]){
  int i;

  // Imprime a quantidade de argumentos
  printf("Quantidade de argumentos: %d\n", argc);

  // Imprime cada argumento passado
  for(i=0; i < argc; i++){
    printf("Argumento %d: %s\n", i, argv[i]);
  }

  return 0;
}
```
Ao executar este código com os argumentos "primeiro", "segundo" e "terceiro" passados na linha de comando, o resultado será:

```
Quantidade de argumentos: 4
Argumento 0: nome-do-programa
Argumento 1: primeiro
Argumento 2: segundo
Argumento 3: terceiro
```

## Aprofundando na leitura de argumentos de linha de comando

Muitas vezes, é necessário ler argumentos de linha de comando e utilizá-los de maneira mais elaborada em um programa. Por exemplo, podemos querer que o usuário passe algum valor numérico e realizar operações com ele.

Para isso, é preciso converter a string contendo o argumento para um tipo numérico, como `int` ou `float`. A biblioteca `stdlib.h` possui funções que facilitam essa conversão, como `atoi()` e `atof()`. No exemplo abaixo, vemos como podemos utilizar essas funções para ler um número passado como argumento e realizar uma soma com outro valor previamente definido no código.

```C
#include <stdio.h>
#include <stdlib.h>

int main(int argc, char *argv[]){
  int num, soma;

  // Lê o argumento passado e converte para inteiro
  num = atoi(argv[1]);
  
  // Realiza a soma com um valor pré-definido
  soma = num + 10;

  // Imprime o resultado
  printf("A soma de %d com 10 é %d\n", num, soma);

  return 0;
}
```
Ao executar este código com o argumento "5" passado na linha de comando, o resultado será:

```
A soma de 5 com 10 é 15
```

## Veja também
- [Documentação oficial do C](https://devdocs.io/c/)
- [Tutorial sobre argumentos de linha de comando em C](https://www.tutorialspoint.com/cprogramming/c_command_line_arguments.htm)
- [Funções de conversão de tipos em C](https://www.geeksforgeeks.org/conversion-functions-in-c/)
- [Outros recursos úteis sobre C](https://www.cprogramming.com/)