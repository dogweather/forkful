---
title:                "Lendo argumentos de linha de comando"
html_title:           "Arduino: Lendo argumentos de linha de comando"
simple_title:         "Lendo argumentos de linha de comando"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/haskell/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## O que & Por quê?
A leitura de argumentos de linha de comando permite a interação dos usuários com os programas através da linha de comando. Fazemos isso para influenciar o comportamento de um programa sem alterar o código em si.

## Como:
Para os exemplos de código em Haskell, vamos usar a função `getArgs` da biblioteca `System.Environment`. Aqui está um exemplo:

```Haskell
import System.Environment

main :: IO ()
main = do
    args <- getArgs
    print args
```

Se você executar este programa com argumentos na linha de comando (por exemplo. `runhaskell Teste.hs ola mundo`), o resultado será:

```Haskell
["ola","mundo"]
```

## Mergulho Profundo:

Ao longo dos anos, a leitura de argumentos da linha de comando tem sido uma importante forma de interação com programas. Isso é particularmente útil para scripts de automação e ferramentas de linha de comando.

No Haskell, além do `getArgs`, existe também o `getProgName` que retorna o nome do programa em execução.

Além disso, o módulo `System.Console.GetOpt` fornece funções para lidar com opções de linha de comando se você precisar de algo mais complexo. 

## Veja Também:

Para um guia mais aprofundado sobre a biblioteca `System.Environment`, consulte a documentação da biblioteca: [System.Environment](https://hackage.haskell.org/package/base-4.14.1.0/docs/System-Environment.html)

Para mais exemplos de como usar `getOpt`, confira este tutorial: [Command line arguments in Haskell with GetOpt](http://hackage.haskell.org/package/base-4.14.1.0/docs/System-Console-GetOpt.html)

Já para um tutorial completo do Haskell, o Learn You a Haskell é um ótimo recurso: [Learn You a Haskell](http://learnyouahaskell.com/)