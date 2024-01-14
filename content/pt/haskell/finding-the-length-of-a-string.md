---
title:    "Haskell: Encontrando o tamanho de uma string"
keywords: ["Haskell"]
---

{{< edit_this_page >}}

## Por que

Encontrar o comprimento de uma string é uma tarefa comum em programação, e entender como fazer isso pode ajudá-lo a escrever códigos mais eficientes e elegantes em Haskell.

## Como fazer

Para encontrar o comprimento de uma string em Haskell, podemos usar a função `length`. Vamos dar uma olhada em um exemplo:

```Haskell
nome = "João"
comprimento = length nome
```

Neste exemplo, definimos a variável `nome` como uma string e, em seguida, usamos a função `length` para encontrar seu comprimento. O resultado será 4, pois a string "João" contém 4 caracteres.

Outra maneira de encontrar o comprimento de uma string é usando a função `foldr`. Esta função recebe uma função e uma lista como argumentos e aplica a função a cada elemento da lista, retornando um único valor. Podemos usar `foldr` para criar nossa própria função `length`:

```Haskell
meuLength xs = foldr (\_ acc -> acc + 1) 0 xs
```

Neste exemplo, usamos uma função anônima `\_ acc -> acc + 1` que recebe um elemento da lista (representado por `_`) e um acumulador (representado por `acc`) e retorna o valor do acumulador mais um. Ao aplicar essa função a cada elemento da lista, obtemos o número total de elementos, que é o comprimento da string. Aqui está um exemplo de como usar essa função:

```Haskell
nome = "Maria"
comprimento = meuLength nome
```

O resultado será novamente 4, pois a string "Maria" também contém 4 caracteres.

## Mergulho Profundo

A função `length` é uma função polimórfica, o que significa que ela pode ser aplicada a diferentes tipos de dados, não apenas strings. Isso é possível porque a função não depende do tipo específico de dado, mas sim da quantidade de elementos em uma estrutura de dados. Isso também é evidente em nossa função `meuLength`, onde usamos `foldr` em vez de apenas somar 1 a um contador, tornando-o mais flexível.

Além disso, é importante notar que a função `length` não é a única maneira de encontrar o comprimento de uma string em Haskell. Por exemplo, também podemos usar a função `Data.List.genericLength` do módulo `Data.List` para encontrar o comprimento de uma string, mas ela deve ser usada com cuidado, pois pode introduzir imprecisões devido ao uso de números racionais em vez de inteiros.

## Veja também

- [Documentação da função `length`](https://hackage.haskell.org/package/base-4.14.1.0/docs/Prelude.html#v:length)
- [Documentação da função `foldr`](https://hackage.haskell.org/package/base-4.14.1.0/docs/Prelude.html#v:foldr)
- [Documentação da função `Data.List.genericLength`](https://hackage.haskell.org/package/base-4.14.1.0/docs/Data-List.html#v:genericLength)