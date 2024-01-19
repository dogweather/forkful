---
title:                "Iniciando um novo projeto"
html_title:           "Javascript: Iniciando um novo projeto"
simple_title:         "Iniciando um novo projeto"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/haskell/starting-a-new-project.md"
---

{{< edit_this_page >}}

---
title: Primeiros passos com Haskell - Iniciando um Projeto
---

## O que & Por quê?

Iniciar um novo projeto envolve planejamento, configuração e a estruturação inicial do código. Programadores fazem isso para organizar logicamente suas ideias e garantir uma base sólida para desenvolver suas aplicações.

## Como fazer:

Haskell utiliza a ferramenta de construção Stack para facilitar o início de novos projetos. 

```Haskell
-- Instale o Stack
stack new meu_projeto
cd meu_projeto
stack setup
stack build
```
Para testar se tudo está funcionando corretamente, vamos imprimir algo no console.

```Haskell
main :: IO ()
main = putStrLn "Olá, Mundo!"

stack exec meu_projeto
-- Saída
-- Olá, Mundo!
```

## Mergulho Deeper

**Contexto histórico:** Haskell é uma linguagem de programação puramente funcional nomeada em homenagem ao lógico Haskell Curry. Seu desenvolvimento começou no final dos anos 80 com o objetivo de consolidar muitas ideias distintas de linguagens funcionais anteriores.

**Alternativas:** Existem outras ferramentas além do Stack, como o Cabal. Contudo, o Stack é geralmente considerado mais amigável, especialmente para iniciantes.

**Detalhes de implementação:** Ao iniciar um novo projeto com Stack, é criada uma estrutura de diretório base, com o arquivo `.cabal`. Esse arquivo contém metadados do projeto e dependências.

## Veja também

Aqui estão alguns links para recursos úteis.

1. [Documentação oficial do Haskell](https://www.haskell.org/documentation/): Para aprofundar seu conhecimento sobre Haskell.
2. [Stack](https://docs.haskellstack.org/en/stable/README/): Para entender mais sobre a ferramenta de construção Haskell Stack.
3. [Aprender Haskell](http://learnyouahaskell.com/): Um bom lugar para começar a aprender Haskell, especialmente para novatos.
---