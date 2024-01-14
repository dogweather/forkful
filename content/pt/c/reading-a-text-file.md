---
title:    "C: Lendo um arquivo de texto"
keywords: ["C"]
---

{{< edit_this_page >}}

## Por que ler um arquivo de texto em C?

Para muitas aplicações, ler um arquivo de texto pode ser uma parte importante do processo de programação em C. Isso permite que o programa acesse dados armazenados em um arquivo externo e os utilize em seu código. Além disso, a leitura de arquivos de texto também é uma habilidade essencial para o desenvolvimento de programas que interagem com usuários, como jogos, aplicativos de linha de comando ou softwares de produtividade.

## Como ler um arquivo de texto em C

Existem várias maneiras de ler um arquivo de texto em um programa em linguagem C. A seguir, serão apresentadas duas das formas mais comuns, mas é importante lembrar que existem outras opções, dependendo do objetivo e das necessidades do projeto.

### Usando a função fopen()

A função ```fopen()``` é uma das mais utilizadas para ler arquivos de texto em C. Ela permite que um arquivo externo seja aberto em um determinado modo de acesso (como leitura, escrita ou adição) e, assim, seja possível realizar operações neste arquivo.

Por exemplo, para abrir um arquivo de texto chamado "texto.txt" em modo de leitura, podemos escrever o seguinte código:

```
#include <stdio.h>

int main(){

    FILE *arquivo;

    arquivo = fopen("texto.txt", "r");

    if (arquivo == NULL){
        printf("Erro ao abrir o arquivo!");
        return 1;
    }

    // código para operações no arquivo

    fclose(arquivo);

    return 0;
}
```

Note que, após abrir o arquivo, é importante sempre fechá-lo utilizando a função ```fclose()```, para garantir que não haja problemas de acesso ao arquivo na execução do programa.

### Usando a função scanf()

A função ```scanf()``` é outra opção bastante utilizada para ler dados de um arquivo de texto em C. Ela lê e armazena os dados no formato especificado pelo programador, sendo especialmente útil para a leitura de informações estruturadas em linhas.

Por exemplo, se tivermos um arquivo de texto que contém as notas de alunos em cada linha no formato "nome, nota1, nota2, nota3", podemos utilizar o seguinte código para ler e imprimir essas informações:

```
#include <stdio.h>

int main(){

    char aluno[50];
    float nota1, nota2, nota3;

    FILE *arquivo;

    arquivo = fopen("notas.txt", "r");

    if (arquivo == NULL){
        printf("Erro ao abrir o arquivo!");
        return 1;
    }

    while(scanf("%s %f %f %f", aluno, &nota1, &nota2, &nota3) != EOF){
        printf("Aluno: %s\n Notas: %.1f %.1f %.1f\n", aluno, nota1, nota2, nota3);
    }

    fclose(arquivo);

    return 0;
}
```

### Como ler múltiplos arquivos de texto

Além de ler um único arquivo de texto, também é possível realizar a leitura de múltiplos arquivos utilizando estruturas de repetição e arrays. Dessa forma, podemos acessar e utilizar dados de diferentes arquivos em uma única execução do programa.

## Mergulho profundo

Para quem deseja se aprofundar ainda mais no tema, existem outras funções e técnicas que podem ser utilizadas para a leitura de arquivos de texto em C. Além disso, é importante ressaltar que é necessário realizar o tratamento de erros e exceções ao ler e manipular arquivos, para garantir a integridade e execução correta do programa.

## Veja também

- [Tutorial de leitura de arquivos em C](https://www.codingame.com/playgrounds/38085/tutorial-de-leitura-e-escrita-de-arquivos-em-c)
- [Documentação oficial da função fopen()](https://www.cplusplus.com/reference/cstdio/fopen/)
- [Diferenças entre ler e escrever em arquivos de texto em C](https://stackoverflow.com/questions/19672060/what-is-the-difference-between-reading-and-writing-from-to-a-file-in-c)