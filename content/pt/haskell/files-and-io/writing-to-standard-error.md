---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:33:18.089492-07:00
description: "Como fazer: Em Haskell, escrever para stderr \xE9 direto com o m\xF3\
  dulo `System.IO` da biblioteca base. Abaixo est\xE1 um exemplo b\xE1sico para demonstrar."
lastmod: '2024-03-13T22:44:46.641696-06:00'
model: gpt-4-0125-preview
summary: "Em Haskell, escrever para stderr \xE9 direto com o m\xF3dulo `System.IO`\
  \ da biblioteca base."
title: "Escrevendo para o erro padr\xE3o"
weight: 25
---

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
