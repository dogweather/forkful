---
title:                "Haskell: Lendo argumentos da linha de comando"
simple_title:         "Lendo argumentos da linha de comando"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/haskell/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Por que

Você pode estar se perguntando por que alguém iria querer ler argumentos da linha de comando em um programa Haskell. A resposta é simples: isso pode tornar seu programa mais interativo e flexível, permitindo que os usuários forneçam opções e variáveis ao executá-lo.

## Como fazer

Para ler argumentos da linha de comando em Haskell, é necessário importar o módulo System.Environment. Em seguida, você pode usar a função `getArgs`, que retorna uma lista de strings contendo os argumentos fornecidos na linha de comando.

Por exemplo, se quisermos ler dois números inteiros da linha de comando e imprimir sua soma, podemos escrever o seguinte código:

```Haskell
import System.Environment

main = do
    args <- getArgs
    let num1 = read (args !! 0) :: Int
        num2 = read (args !! 1) :: Int
        result = num1 + num2
    putStrLn $ "A soma de " ++ show num1 ++ " e " ++ show num2 ++ " é " ++ show result
```

Ao executar o programa com os argumentos "2" e "3", teremos o seguinte resultado:

```
A soma de 2 e 3 é 5
```

É importante lembrar que, por padrão, os argumentos são tratados como strings, por isso é necessário converter para o tipo de dado desejado, no nosso caso, o tipo `Int`.

## Navegação profunda

A função `getArgs` pode retornar não apenas os argumentos fornecidos na linha de comando, mas também variáveis de ambiente e outros parâmetros que podem ser passados ​​para o programa. Além disso, é possível trabalhar com argumentos nomeados, utilizando módulos como o `optparse-applicative` ou `cmdargs`.

Outra dica útil é utilizar a função `lookupEnv` do módulo System.Environment para verificar se uma variável de ambiente específica foi fornecida na linha de comando. Isso pode ser útil para definir opções padrão ou personalizadas para seu programa.

## Veja também

- [Documentação oficial do módulo System.Environment](https://hackage.haskell.org/package/base-4.15.0.0/docs/System-Environment.html)
- [Tutorial sobre leitura de argumentos da linha de comando em Haskell](https://wiki.haskell.org/High-level_option_handling_with_getOpt) 
- [Exemplos práticos de uso do módulo System.Environment](https://www.codementor.io/@undefined/A_1_Novice_s_Taking_Haskell_To_the_Command_Line-z1klr8vxz)