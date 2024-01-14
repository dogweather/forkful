---
title:                "Fish Shell: Começando um novo projeto"
programming_language: "Fish Shell"
category:             "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/fish-shell/starting-a-new-project.md"
---

{{< edit_this_page >}}

## Porque

Este artigo irá mostrar o quão fácil pode ser começar um novo projeto utilizando o Fish Shell. Com o recurso de autocomplete integrado e a estrutura simples de comandos, o Fish Shell torna o processo de criação e gerenciamento de projetos mais eficiente e produtivo. Para aqueles que desejam aumentar sua produtividade e simplificar o desenvolvimento, seguir esses passos simples em Fish Shell é fundamental.

## Como Fazer

Para começar, abra o seu Terminal e inicie o Fish Shell digitando ```fish```.

Para criar um novo diretório para o seu projeto, use o comando ```mkdir``` seguido pelo nome do diretório desejado, como por exemplo: 

```
mkdir novo_projeto
```

Em seguida, navegue para o novo diretório usando o comando ```cd```.

```
cd novo_projeto
```

Agora, usando o recurso de autocomplete do Fish Shell, vamos criar um arquivo de texto dentro do diretório recém-criado. Digite ```vim``` e pressione a tecla TAB duas vezes. O Fish Shell mostrará uma lista de opções de comandos que começam com "vim". Selecione a opção ```vim``` e pressione a tecla ENTER.

```
vim novo_arquivo.txt
```

Isso abrirá o editor de texto Vim. Adicione seu código ou texto desejado e salve o arquivo pressionando as teclas ```esc``` e depois ```:wq``` (salvar e sair). Para executar um comando, basta digitá-lo diretamente no Terminal.

```
ls
```

Isso listará todos os arquivos e diretórios no diretório atual, incluindo o novo arquivo que você criou.

## Profundidade

Embora seja fácil criar um novo projeto usando o Fish Shell, também é importante entender os recursos e comandos adicionais que ele oferece. Alguns comandos úteis para gerenciar e trabalhar com projetos em Fish Shell são:

- ```cp```: copiar arquivos e diretórios
- ```rm```: excluir arquivos e diretórios
- ```mv```: mover arquivos e diretórios
- ```cat```: exibir o conteúdo de um arquivo
- ```grep```: pesquisar por uma determinada palavra ou expressão em um arquivo
- ```echo```: imprimir uma mensagem ou variável na saída

Além disso, Fish Shell possui vários plugins disponíveis para melhorar ainda mais o processo de desenvolvimento e gerenciamento de projetos. É possível encontrar e instalar esses plugins usando o gerenciador de pacotes do Fish Shell, o ```fisher```.

## Veja Também

- [Fish Shell documentation](https://fishshell.com/docs/current/index.html)
- [Fisher plugin manager](https://github.com/jorgebucaran/fisher)
- [10 beginner-friendly Fish Shell commands](https://geekflare.com/fish-shell-commands/)

Agora que você conhece os fundamentos de como iniciar um novo projeto em Fish Shell, pode explorar ainda mais e encontrar maneiras de personalizar e otimizar ainda mais seu fluxo de trabalho. Com uma interface amigável e recursos poderosos, o Fish Shell é uma ótima opção para desenvolvedores que desejam aumentar sua produtividade e eficiência. Experimente e veja como ele pode melhorar sua experiência de programação.