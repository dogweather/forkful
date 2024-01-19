---
title:                "Iniciando um novo projeto"
html_title:           "Javascript: Iniciando um novo projeto"
simple_title:         "Iniciando um novo projeto"
programming_language: "C"
category:             "C"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/c/starting-a-new-project.md"
---

{{< edit_this_page >}}

## O Que & Porquê?

Iniciar um novo projeto em programação C é a criação de um programa ou aplicação do zero. Os programadores fazem isso para resolver um problema, criar uma nova ferramenta ou simplesmente para aprender e desafiar suas habilidades.

## Como Fazer:

Para começar um novo projeto em C, primeiro você precisa de um arquivo. Vamos criar "meu_programa.c" usando um editor de texto:

```C
#include <stdio.h>

int main() {
    printf("Olá Mundo!\n");
    return 0;
}
```

Depois compilamos e executamos este arquivo usando o compilador GCC:

```C
$ gcc meu_programa.c -o meu_programa
$ ./meu_programa
Olá Mundo!
```

Pronto, você já iniciou um novo projeto!

## Aprofundando

1) Contexto Histórico: C foi criada em 1972 pelos programadores Bell Labs, Dennis Ritchie e Ken Thompson. O objetivo era desenvolver um sistema operacional UNIX, mas a linguagem se popularizou e foi adotada em diversos outros projetos.

2) Alternativas: Existem várias outras linguagens de programação com as quais você poderia iniciar um novo projeto, como Python, Java, Ruby, etc. Cada linguagem tem suas próprias vantagens e desvantagens, dependendo do seu objetivo.

3) Detalhes de Implementação: Ao iniciar um novo projeto em C, você precisa entender os conceitos básicos de programação, como loops, estruturas de controle, tipos de dados, etc. Além disso, é essencial conhecer a sintaxe e a estrutura de um programa C.

## Veja Também

- [Manual de Referência do C](https://www.gnu.org/software/gnu-c-manual/gnu-c-manual.html)
- [Tutoriais de Programação C](https://www.learn-c.org)
- [Guia de Estilo de Programação C](https://www.kernel.org/doc/html/v4.10/process/coding-style.html)