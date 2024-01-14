---
title:    "Haskell: Extraindo Substrings"
keywords: ["Haskell"]
---

{{< edit_this_page >}}

## Por que extrair substrings em Haskell?

Extrair substrings é uma habilidade útil para muitos programadores em Haskell. Isso pode ser útil para manipular e processar strings, criar algoritmos eficientes ou até mesmo para lidar com entradas do usuário. Saber como extrair substrings pode melhorar sua eficiência e funcionalidade em projetos de programação em Haskell.

## Como fazer

Para extrair uma substring em Haskell, usamos a função `take` e `drop`. Essas funções aceitam dois argumentos: a quantidade de caracteres que queremos extrair, e a string da qual queremos extrair os caracteres. Abaixo está um exemplo de como extrair uma substring de uma string:

```Haskell
-- define a string
let string = "Olá, Haskell!"

-- extrai a substring "Haskell"
let substring = take 7 (drop 5 string)

-- imprime a substring
putStrLn substring 
-- output: "Haskell"
```

Também podemos usar a função `splitAt` para dividir uma string em duas partes, com base em um índice especificado. Por exemplo:

```Haskell
-- define a string
let string = "Hello, world!"

-- divide a string em "Hello" e "world!"
let (part1, part2) = splitAt 5 string

-- imprime as partes da string
putStrLn part1 
-- output: "Hello"
putStrLn part2 
-- output: "world!"
```

## Mergulho Profundo

Existem muitas outras maneiras de extrair substrings em Haskell, incluindo o uso de expressões regulares, compreensão de listas e a função `takeWhile`. Além disso, existem bibliotecas externas que podem facilitar ainda mais a extração de substrings em Haskell. É importante explorar e experimentar diferentes métodos para encontrar o melhor para sua aplicação específica.

## Veja também

- [Documentação oficial do Haskell sobre funções de strings](https://www.haskell.org/onlinereport/standard-prelude.html#ltyString)
- [Como extrair uma substring em Haskell](https://www.geeksforgeeks.org/haskell-extracting-substring-functions/)
- [Tutorial em vídeo sobre a manipulação de strings em Haskell](https://www.youtube.com/watch?v=fxBuH1jzoIw)