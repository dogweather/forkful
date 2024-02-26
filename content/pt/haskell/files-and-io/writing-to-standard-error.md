---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:33:18.089492-07:00
description: "Escrever para o erro padr\xE3o (stderr) em Haskell permite que os programas\
  \ diferenciem sua sa\xEDda entre resultados normais e mensagens de erro. Isso \xE9\
  \u2026"
lastmod: '2024-02-25T18:49:44.262698-07:00'
model: gpt-4-0125-preview
summary: "Escrever para o erro padr\xE3o (stderr) em Haskell permite que os programas\
  \ diferenciem sua sa\xEDda entre resultados normais e mensagens de erro. Isso \xE9\
  \u2026"
title: "Escrevendo para o erro padr\xE3o"
---

{{< edit_this_page >}}

## O Que & Por Que?
Escrever para o erro padrão (stderr) em Haskell permite que os programas diferenciem sua saída entre resultados normais e mensagens de erro. Isso é crucial para sinalizar problemas e depurar, sem encher a saída padrão (stdout) que muitas vezes carrega os dados principais ou resultado do programa.

## Como fazer:
Em Haskell, escrever para stderr é direto com o módulo `System.IO` da biblioteca base. Abaixo está um exemplo básico para demonstrar:

```haskell
import System.IO

main :: IO ()
main = do
  hPutStrLn stderr "Esta é uma mensagem de erro."
```

A saída deste programa para stderr seria:

```
Esta é uma mensagem de erro.
```

Se você está trabalhando em uma aplicação mais complexa, ou se precisa de melhor controle sobre o registro de atividades (incluindo erros), você pode optar por uma biblioteca de terceiros. Uma escolha popular é `monad-logger` que se integra com o estilo `mtl` de programação em Haskell. Aqui está um pequeno trecho usando `monad-logger`:

```haskell
{-# LANGUAGE OverloadedStrings #-}
import Control.Monad.Logger

main :: IO ()
main = runStderrLoggingT $ do
  logErrorN "Esta é uma mensagem de erro usando monad-logger."
```

Quando executado, a versão `monad-logger` similarmente produz uma mensagem de erro, mas ela é equipada com mais contexto como carimbos de data/hora ou níveis de registro, dependendo da configuração:

```
[Error] Esta é uma mensagem de erro usando monad-logger.
```

Ambos os métodos servem ao propósito de escrever para stderr, com a escolha dependendo em grande parte da complexidade e das necessidades da sua aplicação.
