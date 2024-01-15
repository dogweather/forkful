---
title:                "Começando um novo projeto"
html_title:           "Haskell: Começando um novo projeto"
simple_title:         "Começando um novo projeto"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/haskell/starting-a-new-project.md"
---

{{< edit_this_page >}}

## Por que

Já pensou em aprender uma nova linguagem de programação, mas ainda não sabe qual? Talvez o Haskell seja a resposta para suas necessidades! Além de ser uma linguagem funcional elegante e expressiva, ela também é conhecida por ser extremamente poderosa e segura. Então, se você está procurando iniciar um novo projeto, o Haskell pode ser uma ótima escolha.

## Como Fazer

Começar um novo projeto em Haskell pode parecer intimidador à primeira vista, mas não se preocupe! A seguir, vamos guiar você pelo processo de configuração do ambiente de desenvolvimento e criação do seu primeiro projeto.

### Configurando o Ambiente

Antes de começar a programar em Haskell, é necessário ter o ambiente de desenvolvimento configurado em sua máquina. Existem várias opções disponíveis, mas a mais popular é o GHC (Glasgow Haskell Compiler). Para instalá-lo, você pode seguir as instruções específicas do seu sistema operacional no [site oficial do GHC](https://www.haskell.org/ghc/).

### Criando um Projeto

Agora que o GHC está instalado, é hora de criar um novo projeto Haskell. Existem muitas ferramentas disponíveis para ajudá-lo nesse processo, mas uma das mais comuns é o Cabal. Para criar um novo projeto com ele, basta seguir os passos abaixo:

1. Abra o terminal ou prompt de comando e navegue até a pasta onde deseja criar seu projeto.
2. Execute o comando `cabal init` e preencha as informações solicitadas, como nome do projeto, autor, etc. 
3. Uma vez que o processo de inicialização do Cabal estiver concluído, você terá uma estrutura básica do seu projeto pronta para uso.

Parabéns, você acaba de criar seu primeiro projeto Haskell!

### Hello World em Haskell

Agora que temos o projeto criado, vamos escrever nosso primeiro programa Haskell, o famoso "Hello World". Abra o arquivo `Main.hs` gerado pelo Cabal e adicione o seguinte código:

```Haskell
main = putStrLn "Hello World!"
```

Esse código simplesmente imprime a mensagem "Hello World!" na tela. Para testá-lo, basta salvar o arquivo e executar o comando `cabal run` no terminal. Você deverá ver a mensagem sendo exibida na tela.

## Deep Dive

Além das ferramentas mencionadas acima, também é importante mencionar o Stack, uma ferramenta de automação de construção e gerenciamento de dependências para projetos Haskell. Ele é amplamente utilizado e altamente recomendado para gerenciar projetos maiores e mais complexos.

Outro ponto importante quando se inicia um projeto Haskell é conhecer e adotar as melhores práticas de codificação, como o uso de tipos fortemente definidos e funções puras. Isso garantirá que seu código seja seguro e fácil de manter.

Haskell também possui uma comunidade ativa e prestativa, com vários fóruns, listas de discussão e canais de chat disponíveis para ajudá-lo em sua jornada de aprendizagem.

## Veja Também

- [Site oficial do GHC](https://www.haskell.org/ghc/)
- [Documentação oficial do Cabal](https://cabal.readthedocs.io/en/latest/)
- [Site oficial do Stack](https://docs.haskellstack.org/en/stable/README/)