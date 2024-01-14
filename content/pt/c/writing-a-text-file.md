---
title:                "C: Escrevendo um arquivo de texto"
simple_title:         "Escrevendo um arquivo de texto"
programming_language: "C"
category:             "C"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/c/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Por que escrever um arquivo de texto?

Se você está lendo este blog, é provável que esteja interessado em aprender a programar em C. Uma das tarefas mais básicas e importantes que um programador precisa saber é como escrever um arquivo de texto através do código. Neste post, vamos explicar por que escrever um arquivo de texto é importante e como você pode fazer isso em um programa escrito em C.

## Como escrever um arquivo de texto?

Para escrever um arquivo de texto em um programa C, você precisará seguir algumas etapas simples. Primeiro, você precisa incluir a biblioteca "stdio.h", que é responsável por funções de entrada e saída de dados. Em seguida, declare a variável do tipo "FILE", que será usada para abrir e manipular o arquivo. Em seguida, crie o arquivo usando a função "fopen()" e forneça o nome do arquivo e o modo de abertura, que pode ser "w" para escrita. Agora você está pronto para escrever no arquivo usando a função "fprintf()". Por fim, feche o arquivo usando a função "fclose()" para garantir que todas as alterações sejam salvas.

Aqui está um exemplo de código que cria um arquivo de texto chamado "arquivo.txt" e escreve uma frase nele:

```C
#include <stdio.h>

int main(){
    FILE *arquivo;
    arquivo = fopen("arquivo.txt", "w");
    fprintf(arquivo, "Este é um exemplo de frase escrita em um arquivo de texto.");
    fclose(arquivo);
    return 0;
}
```

Ao executar este código, você verá que um novo arquivo de texto foi criado e que a frase foi escrita nele.

## Mergulho mais profundo

Agora que você já sabe como escrever um arquivo de texto em um programa em C, vamos dar um mergulho mais profundo neste assunto. Existem algumas coisas que você precisa ter em mente ao escrever um arquivo de texto. Em primeiro lugar, é importante garantir que o arquivo tenha sido aberto com sucesso antes de escrever nele. Você pode verificar isso verificando se a variável "arquivo" não é igual a "NULL". Além disso, se você estiver escrevendo várias linhas no arquivo, lembre-se de adicionar o caractere de nova linha "\n" no final de cada linha para evitar que todas as palavras fiquem em uma única linha.

Outra coisa importante é lembrar de fechar o arquivo depois de terminar de escrever nele, pois manter o arquivo aberto pode causar problemas de memória.

## Veja também

- [Tutorial de C para iniciantes](https://www.cprogressivo.net/)
- [Documentação da função fopen()](https://www.tutorialspoint.com/c_standard_library/c_function_fopen.htm)
- [Artigo sobre escrita de arquivos em C](https://www.geeksforgeeks.org/writing-files-in-c/)