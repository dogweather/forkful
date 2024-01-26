---
title:                "Verificando se um diretório existe"
date:                  2024-01-20T14:56:47.708670-07:00
html_title:           "Fish Shell: Verificando se um diretório existe"
simple_title:         "Verificando se um diretório existe"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/haskell/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## O que é & Por quê?
Verificar se um diretório existe é checar se, no sistema de arquivos, uma certa pasta está lá. Programadores fazem isso para evitar erros ao tentar acessar, ler ou escrever em diretórios que não existem.

## Como fazer:
Tá na hora de botar a mão na massa. Com Haskell, você usa a biblioteca `directory` para saber se um diretório tá no ar. Veja como:

```haskell
import System.Directory (doesDirectoryExist)

main :: IO ()
main = do
    let dir = "pastaTeste"
    exists <- doesDirectoryExist dir
    putStrLn $ "O diretório " ++ dir ++ (if exists then " existe." else " não existe.")
```

Execute isso e obtenha:

```
O diretório pastaTeste existe.
```
ou
```
O diretório pastaTeste não existe.
```
dependendo se a pasta está lá ou não.

## Aprofundamento
Lá atrás, antes das bibliotecas modernas, verificar diretórios em Haskell podia ser um parto. Mas hoje, com a `directory` library, a vida tá fácil. Alternativas? Você pode usar `System.FilePath` para mexer com paths de uma forma mais pura. Agora, por dentro, `doesDirectoryExist` chama funções do sistema operacional para verificar a existência do diretório. É simples, direto e não precisa reinventar a roda.

## Veja também:
- Para mais sobre a biblioteca `directory`, consulta: [Hackage - Directory](https://hackage.haskell.org/package/directory)
- Um guia de `System.FilePath` para os nitty-gritty de paths: [Hackage - System.FilePath](https://hackage.haskell.org/package/filepath)

Ler essas fontes vai te dar uma base forte e vai preparar você para evitar aqueles erros chatos de diretório no seu próximo projeto Haskell.
