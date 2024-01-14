---
title:    "Haskell: Lendo argumentos da linha de comando"
keywords: ["Haskell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/haskell/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Por que?

Se você já trabalha com Haskell, provavelmente já ouviu falar sobre argumentos da linha de comando e como eles podem ser úteis em suas aplicações. No entanto, para aqueles que são novos na linguagem, pode não ser óbvio por que essa habilidade é tão importante. Neste artigo, vamos explorar o porquê de ler argumentos da linha de comando pode ser uma habilidade valiosa para qualquer programador Haskell.

## Como fazer

Ler argumentos da linha de comando em Haskell é relativamente simples. Primeiro, precisamos importar o módulo `System.Environment` para ter acesso às funções que nos permitem ler e manipular os argumentos. Em seguida, utilizamos a função `getArgs` para receber uma lista de argumentos fornecida pelo usuário na linha de comando. Vamos ver um exemplo:

```Haskell
import System.Environment

main = do
  args <- getArgs
  print args
```

Se executarmos este código no terminal e fornecermos alguns argumentos, por exemplo: `runhaskell read_args.hs arg1 arg2`, o resultado será uma lista contendo esses argumentos `["arg1", "arg2"]`.

No entanto, a lista de argumentos fornecida pela função `getArgs` é do tipo `IO [String]`, o que pode ser um pouco confuso para os iniciantes em Haskell. Mas não se preocupe, basta pensar em `IO` como uma ação que será executada pelo compilador. Para conseguirmos trabalhar com essa lista, usamos a desestruturação de `IO` com a função de ordem superior `map` para converter cada elemento da lista em um valor do tipo `String`. Vamos ver como ficaria o código:

```Haskell
import System.Environment

main = do
  args <- getArgs
  let argsList = map show args
  print argsList
```
O resultado será uma lista contendo os argumentos lidos da linha de comando no formato `String`, o que nos permite manipulá-los em nossos programas.

## Mergulho Profundo

Agora que já sabemos como ler os argumentos da linha de comando em Haskell, podemos nos aprofundar um pouco mais e entender melhor como essa habilidade pode ser útil em nossos programas. Alguns casos de uso comuns são:

- Configurações de programas: podemos fornecer opções e configurações para nossos programas através da linha de comando, tornando-os mais flexíveis e customizáveis.
- Depuração e testes: ao ler argumentos da linha de comando, podemos testar diferentes cenários e utilizar argumentos de entrada específicos para depurar e encontrar possíveis erros em nossos programas.
- Integração com outros programas: se nosso programa precisa interagir com outros softwares, podemos utilizar argumentos da linha de comando para passar informações e realizar a comunicação entre eles.

## Veja também

- [Documentação do módulo `System.Environment`](https://hackage.haskell.org/package/base-4.12.0.0/docs/System-Environment.html)
- [Aprenda Haskell (em inglês)](https://learn.hfm.io)
- [Curso de Programação Funcional (em português)](https://www.coursera.org/learn/programacao-funcional)