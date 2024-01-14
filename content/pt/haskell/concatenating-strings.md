---
title:    "Haskell: Unindo strings"
keywords: ["Haskell"]
---

{{< edit_this_page >}}

## Por que

Se você é novo no mundo da programação em Haskell, pode estar se perguntando por que as pessoas costumam usar concatenação de strings. A resposta simples é que a concatenação de strings é uma maneira de juntar duas ou mais strings para formar uma única string.

## Como Fazer

Para fazer a concatenação de strings em Haskell, podemos usar a função `++`. Por exemplo, digamos que temos duas strings, "Olá" e "mundo". Se quisermos criar uma nova string com estas duas palavras juntas, podemos usar a seguinte linha de código:

```Haskell
"Olá" ++ "mundo"
```

A saída deste exemplo seria `"Olá mundo"`. Como podemos ver, a função `++` simplesmente adiciona uma string à outra. 

Mas isso não é tudo! Também podemos usar a concatenação para juntar uma string a uma lista de strings. Por exemplo, se tivermos a lista `["Eu", "amo", "Haskell"]` e quisermos criar uma única string com todas essas palavras juntas e separadas por espaços, podemos usar o seguinte código:

```Haskell
" " ++ ["Eu", "amo", "Haskell"]
```

A saída seria `"Eu amo Haskell"`, com a string vazia servindo como o separador entre as palavras da lista.

## Mergulho Profundo

Para aqueles que estão interessados em aprofundar-se mais no tema da concatenação de strings em Haskell, existem algumas coisas importantes a serem mencionadas. A primeira é que a função `++` é associativa à esquerda, o que significa que, se tivermos mais de duas strings sendo concatenadas, a função irá agrupar as strings da esquerda para a direita. Por exemplo:

```Haskell
"Olá" ++ " " ++ "mundo" ++ "!" 
```

Seria equivalente a:

```Haskell
("Olá" ++ " ") ++ "mundo" ++ "!" 
```

O segundo ponto importante é que, ao contrário de outras linguagens de programação, a concatenação de strings em Haskell é uma operação relativamente custosa. Isso ocorre porque, em Haskell, strings são representadas como listas de caracteres, e a concatenação de listas requer uma cópia completa da lista original. Portanto, ao lidar com grandes quantidades de dados, pode ser mais eficiente usar operações de concatenação otimizadas, como a função `concat`.

## Veja Também

- [Documentação sobre concatenação de strings em Haskell](https://hackage.haskell.org/package/base-4.15.0.0/docs/Data-List.html#g:13)
- [Tutorial básico sobre strings em Haskell](https://riptutorial.com/pt/haskell/topic/8743/strings)
- [Explicações sobre a associação à esquerda em operações de concatenação](https://stackoverflow.com/questions/17102746/concatenation-associativity-in-haskell)