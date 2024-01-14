---
title:    "Haskell: Lendo argumentos da linha de comando"
keywords: ["Haskell"]
---

{{< edit_this_page >}}

## Por que ler argumentos de linha de comando em Haskell?

Ler argumentos de linha de comando é uma habilidade importante para qualquer programador Haskell, pois permite que você crie programas mais interativos e dinâmicos. Ao entender como ler e usar os argumentos de linha de comando, você pode tornar seus programas mais flexíveis e adaptáveis às necessidades do usuário final.

## Como fazer em Haskell?

Para ler argumentos de linha de comando em Haskell, podemos usar a função "getArgs" do módulo System.Environment. Esta função retorna uma lista de argumentos passados para o programa como strings. O código abaixo mostra um exemplo simples de como ler e imprimir os argumentos de linha de comando:

```Haskell
import System.Environment

main = do
    args <- getArgs
    print $ "Argumentos de linha de comando: " ++ show args
```

Se executarmos este programa no terminal com os argumentos "Olá" e "Mundo", o resultado será:

```
Argumentos de linha de comando: ["Olá", "Mundo"]
```

Podemos então manipular essa lista de argumentos da forma que desejarmos, seja imprimindo cada um deles individualmente ou utilizando-os como entrada para outras funções.

## Mais informações sobre a leitura de argumentos de linha de comando

Além da função "getArgs", o módulo System.Environment também possui outras funções úteis para trabalhar com argumentos de linha de comando, como "getProgName" para obter o nome do programa e "getEnv" para acessar variáveis de ambiente. Além disso, é possível utilizar a biblioteca "optparse-applicative" para criar uma interface de linha de comando mais robusta e fácil de usar para seus programas em Haskell.

## Veja também
- [Documentação do módulo System.Environment](https://hackage.haskell.org/package/base-4.15.0.0/docs/System-Environment.html)
- [Documentação do módulo Options.Applicative](https://hackage.haskell.org/package/optparse-applicative)
- [Tutorial sobre leitura de argumentos de linha de comando em Haskell](https://www.tutorialspoint.com/haskell/haskell_command_line_arguments.htm)