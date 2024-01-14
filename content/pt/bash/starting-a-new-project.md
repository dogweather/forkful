---
title:    "Bash: Iniciando um novo projeto"
keywords: ["Bash"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/bash/starting-a-new-project.md"
---

{{< edit_this_page >}}

## Porque

Quando se trata de aprender uma nova linguagem de programação, pode ser intimidante dar os primeiros passos para criar um novo projeto. No entanto, o Bash é uma linguagem de script simples e poderosa que pode ser usada para automatizar tarefas e facilitar a sua vida como programador. Então, por que não mergulhar nessa aventura e começar a criar seu próprio projeto do zero?

## Como Iniciar um Novo Projeto em Bash

Para iniciar um novo projeto em Bash, você precisará seguir algumas etapas simples:

1. Crie uma nova pasta para o seu projeto
2. Inicie um novo arquivo de script com a extensão ".sh"
3. Adicione a ela as permissões de execução usando o comando `chmod +x <nome do arquivo>`
4. Comece a escrever o seu código no arquivo de script usando um editor de texto ou a linha de comando

Para executar seu projeto, basta digitar `./<nome do arquivo>` na linha de comando. Você também pode passar argumentos para o seu script, como `./<nome do arquivo> argumento1 argumento2`.

Aqui está um exemplo simples de um script em Bash que pede ao usuário para digitar seu nome e, em seguida, o saúda:

```Bash
#!/bin/bash

echo "Qual é o seu nome?"
read nome

echo "Olá, $nome! Bem-vindo ao meu novo projeto em Bash."
```

A saída deste script seria:

```Bash
Qual é o seu nome?
João
Olá, João! Bem-vindo ao meu novo projeto em Bash.
```

## Mergulho Profundo

Uma das coisas mais importantes ao iniciar um novo projeto em Bash é entender a diferença entre as variáveis internas e as variáveis criadas pelo usuário. As variáveis internas são fornecidas pelo Bash e podem ser usadas para tarefas específicas, como armazenar informações de usuário ou valores de retorno de comandos. Por outro lado, as variáveis criadas pelo usuário são criadas pelo próprio programador para armazenar qualquer tipo de informação necessária para o projeto.

Outro conceito importante em Bash é o uso de estruturas de controle, como o "if", "for" e "while", para controlar o fluxo do seu programa. Além disso, é importante entender como os comandos são executados em Bash e como o redirecionamento de entrada e saída pode ser usado para manipular o fluxo do seu programa.

Não se preocupe se você ainda não entende completamente esses conceitos. A prática leva à perfeição e, à medida que você continuar a trabalhar em seus projetos em Bash, você ficará cada vez mais confortável com esses conceitos.

## Veja também

- [Documentação oficial do Bash](https://www.gnu.org/software/bash/manual/bash.html)
- [Curso de Bash básico](https://www.udemy.com/course/bash-beginner/)
- [Exemplos de scripts em Bash](https://bash.cyberciti.biz/guide/Main_Page)