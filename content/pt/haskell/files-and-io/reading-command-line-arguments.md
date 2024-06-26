---
date: 2024-01-20 17:56:16.539737-07:00
description: "Como fazer: Se voc\xEA salvar isso como `Args.hs` e rod\xE1-lo com `runhaskell\
  \ Args.hs um dois tr\xEAs`, voc\xEA ter\xE1 o seguinte resultado."
lastmod: '2024-04-05T21:53:46.986036-06:00'
model: gpt-4-1106-preview
summary: "Se voc\xEA salvar isso como `Args.hs` e rod\xE1-lo com `runhaskell Args.hs\
  \ um dois tr\xEAs`, voc\xEA ter\xE1 o seguinte resultado."
title: Lendo argumentos da linha de comando
weight: 23
---

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
