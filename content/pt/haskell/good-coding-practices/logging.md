---
date: 2024-01-26 01:04:33.123292-07:00
description: "Log em programa\xE7\xE3o \xE9 basicamente deixar um rastro de migalhas\
  \ na forma de eventos ou mensagens gravadas, que podem ser usadas para acompanhar\
  \ o que sua\u2026"
lastmod: '2024-03-13T22:44:46.631572-06:00'
model: gpt-4-1106-preview
summary: "Log em programa\xE7\xE3o \xE9 basicamente deixar um rastro de migalhas na\
  \ forma de eventos ou mensagens gravadas, que podem ser usadas para acompanhar o\
  \ que sua\u2026"
title: Registro de Logs
weight: 17
---

## O Quê & Por Quê?
Log em programação é basicamente deixar um rastro de migalhas na forma de eventos ou mensagens gravadas, que podem ser usadas para acompanhar o que sua aplicação está fazendo em qualquer momento. Programadores fazem isso para depurar problemas, monitorar o desempenho do sistema e auditar o comportamento por motivos de segurança e conformidade.

## Como fazer:
Em Haskell, log pode ser implementado usando bibliotecas como `monad-logger` ou `hslogger`. Aqui está um exemplo rápido usando `monad-logger`:

```Haskell
{-# LANGUAGE OverloadedStrings #-}

import Control.Monad.Logger
import Control.Monad.IO.Class (liftIO)

logExample :: LoggingT IO ()
logExample = do
    logInfoN "Iniciando a aplicação..."
    liftIO $ putStrLn "Realizando um trabalho crítico..."
    logErrorN "Ops! Algo deu errado."

main :: IO ()
main = runStdoutLoggingT logExample

{- Saída de Exemplo
[Info] Iniciando a aplicação...
Realizando um trabalho crítico...
[Error] Ops! Algo deu errado.
-}
```

Este exemplo simples demonstra como você pode espalhar declarações de log pelo seu código para obter insights sobre o que está acontecendo em tempo de execução. `logInfoN` e `logErrorN` são usados para registrar mensagens informativas e de erro, respectivamente.

## Aprofundamento:
Log evoluiu muito desde simples comandos de impressão até frameworks de log sofisticados. Historicamente, logs eram apenas saídas de texto para um console ou arquivo, mas agora eles incluem dados estruturados que podem ser analisados e processados por diversas ferramentas.

Em Haskell, log pode ser feito em um estilo funcional puro que envolve a passagem explícita de ações de log ou usando contextos monádicos para impureza, onde os registradores são implicitamente encadeados através do cálculo.

A biblioteca `hslogger`, por exemplo, é mais tradicional e mutável comparada ao `monad-logger`. `monad-logger` oferece integração com a pilha de monads e fornece mais flexibilidade em termos de formatação de saída e controle. Ambas as bibliotecas permitem que você defina níveis de log, que ajudam a filtrar mensagens de log com base na sua importância. Os níveis de log incluem debug, info, notice, warning, error, critical, alert e emergency.

A abordagem de Haskell para log muitas vezes está alinhada com sua ênfase em segurança de tipo e pureza. Logs podem ser tratados de uma maneira que, mesmo que o log falhe, não causará a falha da aplicação principal devido às robustas capacidades de tratamento de erro de Haskell.

## Veja Também:
- [Documentação do `monad-logger` no Hackage](https://hackage.haskell.org/package/monad-logger)
- [Pacote `hslogger` no Hackage](https://hackage.haskell.org/package/hslogger)
- [Real World Haskell, Capítulo 19, sobre Tratamento de Erro](http://book.realworldhaskell.org/read/error-handling.html)
- [A Fachada de Log para Haskell (log-base)](https://hackage.haskell.org/package/log-base)
