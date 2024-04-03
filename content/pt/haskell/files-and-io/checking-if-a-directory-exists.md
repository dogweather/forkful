---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:07:27.523475-07:00
description: "Verificar se um diret\xF3rio existe \xE9 uma opera\xE7\xE3o fundamental\
  \ em muitas tarefas de programa\xE7\xE3o, permitindo a\xE7\xF5es condicionais com\
  \ base na presen\xE7a ou\u2026"
lastmod: '2024-03-13T22:44:46.639478-06:00'
model: gpt-4-0125-preview
summary: "Verificar se um diret\xF3rio existe \xE9 uma opera\xE7\xE3o fundamental\
  \ em muitas tarefas de programa\xE7\xE3o, permitindo a\xE7\xF5es condicionais com\
  \ base na presen\xE7a ou aus\xEAncia de estruturas de diret\xF3rio."
title: "Verificando se um diret\xF3rio existe"
weight: 20
---

## Como fazer:
Haskell, através de sua biblioteca base, oferece maneiras diretas de verificar a existência de diretórios, principalmente usando o módulo `System.Directory`. Vamos olhar para um exemplo básico:

```haskell
import System.Directory (doesDirectoryExist)

main :: IO ()
main = do
  let dirPath = "/caminho/para/seu/diretório"
  exists <- doesDirectoryExist dirPath
  putStrLn $ "O diretório existe? " ++ show exists
```

Saída de exemplo, dependendo se o diretório existe:

```
O diretório existe? True
```
Ou:
```
O diretório existe? False
```

Para cenários mais complexos ou funcionalidades adicionais, você pode considerar uma biblioteca de terceiros popular como `filepath` para manipular e tratar caminhos de arquivos de uma maneira mais abstrata. No entanto, para o propósito de simplesmente verificar se um diretório existe, o `System.Directory` da biblioteca base é suficiente e eficiente.

Lembre-se, trabalhar com sistemas de arquivos pode variar entre plataformas, e a abordagem de Haskell visa abstrair algumas dessas diferenças. Sempre teste suas operações de arquivo no sistema alvo para garantir o comportamento esperado.
