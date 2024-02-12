---
title:                "Lendo argumentos da linha de comando"
aliases:
- pt/haskell/reading-command-line-arguments.md
date:                  2024-01-20T17:56:16.539737-07:00
model:                 gpt-4-1106-preview
simple_title:         "Lendo argumentos da linha de comando"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/haskell/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## O quê & Por quê?
Ler argumentos da linha de comando é o ato de pegar informações digitadas pelo usuário quando eles executam seu programa. Programadores fazem isso para tornar os programas interativos e flexíveis às necessidades do usuário, adaptando a execução conforme os dados fornecidos.

## Como fazer:
```Haskell
import System.Environment (getArgs)

main :: IO ()
main = do
    args <- getArgs
    putStrLn $ "Olá! Você passou os seguintes argumentos: " ++ show args
```

Se você salvar isso como `Args.hs` e rodá-lo com `runhaskell Args.hs um dois três`, você terá o seguinte resultado:

```
Olá! Você passou os seguintes argumentos: ["um", "dois", "três"]
```

## Mergulho Profundo
Historicamente, a capacidade de ler argumentos da linha de comando remonta ao início dos sistemas operacionais, permitindo que os usuários interajam diretamente com os programas. No contexto do Haskell, a biblioteca `System.Environment` é usada comumente, mas existem alternativas, como `optparse-applicative`, para uma análise mais sofisticada de argumentos. 

No que diz respeito à implementação, `getArgs` é uma função que retorna uma lista de strings (`[String]`), onde cada string é um argumento passado para o programa. Cada argumento é separado por espaços, a menos que esteja entre aspas, permitindo argumentos que contêm espaços.

## Veja também
- Tutorial oficial da Haskell: [https://www.haskell.org/tutorial/io.html](https://www.haskell.org/tutorial/io.html)
- Documentação da System.Environment: [https://hackage.haskell.org/package/base/docs/System-Environment.html](https://hackage.haskell.org/package/base/docs/System-Environment.html)
- Para uma abordagem mais avançada, `optparse-applicative`: [https://hackage.haskell.org/package/optparse-applicative](https://hackage.haskell.org/package/optparse-applicative)
