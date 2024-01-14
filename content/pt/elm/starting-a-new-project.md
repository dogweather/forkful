---
title:                "Elm: Iniciando um novo projeto"
programming_language: "Elm"
category:             "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/elm/starting-a-new-project.md"
---

{{< edit_this_page >}}

## Porquê

Se você é um desenvolvedor em busca de uma linguagem de programação funcional moderna e eficiente, então pode considerar começar seu próximo projeto em Elm. Com uma sintaxe simples e forte sistema de tipos, Elm é uma excelente escolha para construir aplicações web escaláveis e confiáveis.

## Como

Para iniciar um novo projeto em Elm, você precisa primeiro instalar o compilador Elm e configurar seu ambiente de desenvolvimento. Em seguida, use o comando `elm init` para criar uma nova estrutura de diretórios para o seu projeto. Dentro dessa estrutura, você pode começar a escrever seu código Elm em arquivos `.elm`, que serão compilados em JavaScript e executados no navegador.

Vamos dar uma olhada em um exemplo simples de código Elm:

```Elm
-- Declarando uma função
saudar nome =
    "Olá, " ++ nome ++ "!"

-- Chamando a função
main =
    saudar "João"
```

A saída deste código será uma simples mensagem "Olá, João!". Perceba como a sintaxe de Elm é semelhante a outras linguagens funcionais como Haskell e F#, tornando-a fácil de aprender.

## Mergulho Profundo

Ao iniciar um novo projeto em Elm, é importante entender alguns conceitos-chave, como o padrão de arquitetura “Model-View-Update” e como usar os tipos e funcionalidades do sistema de tipos de Elm para garantir um código seguro e sem erros. Também é útil explorar os pacotes da comunidade Elm para encontrar bibliotecas e ferramentas para ajudá-lo no desenvolvimento.

## Ver também

- [Documentação oficial do Elm](https://elm-lang.org/docs)
- [Pacotes da comunidade Elm](https://package.elm-lang.org/)
- [Tutorial Elm em Português](https://medium.com/@allanmarquesdesouza/elm-a-linguagem-funcional-para-web-bd7f0c631ba0)