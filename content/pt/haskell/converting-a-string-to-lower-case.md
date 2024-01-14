---
title:    "Haskell: Convertendo uma string para minúsculas"
keywords: ["Haskell"]
---

{{< edit_this_page >}}

## Por que?

Convertendo uma sequência de caracteres (string) para letras minúsculas é uma tarefa comum na programação, seja para padronização de dados ou para facilitar a comparação de strings. Aprender como fazer isso em Haskell pode ser útil para uma ampla gama de projetos.

## Como Fazer

```Haskell
import Data.Char (toLower)

-- função que converte uma string para lower case
lowerCase :: String -> String 
lowerCase = map toLower

-- exemplo de uso
main = do 
    let str = "Ola, MUNDO!"
    putStrLn $ "String original: " ++ str
    putStrLn $ "String em lower case: " ++ lowerCase str
```

```
String original: Ola, MUNDO!
String em lower case: ola, mundo!
```

A função `lowerCase` utiliza a função `map` para aplicar a função `toLower` em cada caractere da string. Assim, todas as letras maiúsculas são convertidas para minúsculas.

## Mergulho Profundo

Em Haskell, strings são representadas como listas de caracteres. Por isso, é possível utilizar funções de listas para manipular strings. No exemplo acima, a função `map` foi usada para aplicar uma transformação em todos os elementos de uma lista, o que inclui cada caractere da string.

Além disso, a função `toLower` é apenas uma função de ordem superior que aplica a operação de conversão de maiúsculas para minúsculas a um caractere. Isso significa que, se necessário, é possível criar uma função personalizada de conversão utilizando a função `ord` para obter o número Unicode de um caractere maiúsculo e subtrair 32 para obter o correspondente caractere minúsculo.

## Veja Também

- [Funções built-in em Haskell](https://hackage.haskell.org/package/base-4.15.0.0/docs/Prelude.html)
- [Tutorial de Haskell no Wikilivros](https://pt.wikibooks.org/wiki/Haskell)
- [Manipulando strings em Haskell](https://wiki.haskell.org/Manipulando_strings)