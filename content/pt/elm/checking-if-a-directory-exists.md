---
title:    "Elm: Verificando se um diretório existe"
keywords: ["Elm"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/elm/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Por que verificar se um diretório existe?

Ao desenvolver um aplicativo ou site em Elm, muitas vezes precisamos verificar se um diretório específico existe no sistema de arquivos. Isso pode ser útil para garantir que os usuários tenham acesso a determinados arquivos ou para organizar melhor a estrutura dos arquivos no aplicativo.

## Como fazer:

Verificar a existência de um diretório em Elm é muito simples. Podemos usar a função `File.exists` do pacote `elm/file` para verificar se um determinado diretório existe ou não. Por exemplo:

```Elm
import File exposing (exists)

directoryExists : Bool
directoryExists =
    exists "caminho/do/diretorio"
```

O código acima irá retornar um valor booleano, indicando se o diretório especificado existe ou não. Podemos então usar esse valor em nossa lógica para realizar ações diferentes dependendo da existência do diretório.

## Mergulho profundo:

Além do pacote `elm/file`, também podemos usar o pacote `elm/directory` para trabalhar com diretórios em Elm. Esse pacote nos fornece funções mais avançadas para manipular diretórios, como listar os arquivos contidos em um diretório ou criar um novo diretório.

A função `Directory.exists` do pacote `elm/directory` é semelhante à `File.exists`, mas ela permite verificar a existência de um diretório recursivamente, ou seja, incluindo todos os diretórios aninhados. Isso pode ser útil em casos em que precisamos ter certeza de que um diretório e todos os seus subdiretórios existem antes de realizar uma tarefa específica.

## Veja também:

- Documentação do pacote `elm/file`: [https://package.elm-lang.org/packages/elm/file/latest/](https://package.elm-lang.org/packages/elm/file/latest/)
- Documentação do pacote `elm/directory`: [https://package.elm-lang.org/packages/elm/directory/latest/](https://package.elm-lang.org/packages/elm/directory/latest/)
- Exemplo de código no Ellie App: [https://ellie-app.com/82dDKftKvF3a1](https://ellie-app.com/82dDKftKvF3a1)

Se você precisar trabalhar com diretórios em seu próximo projeto em Elm, esperamos que este artigo tenha sido útil e que os links fornecidos também sejam úteis para o seu aprendizado contínuo da linguagem. Boa codificação!