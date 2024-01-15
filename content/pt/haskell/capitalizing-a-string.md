---
title:                "Convertendo Uma String em Maiúsculas"
html_title:           "Haskell: Convertendo Uma String em Maiúsculas"
simple_title:         "Convertendo Uma String em Maiúsculas"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/haskell/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Por que

Você já se deparou com uma situação em que precisou capitalizar uma string em seu código Haskell? Talvez você esteja lidando com dados de entrada de usuários ou precisa formatar uma saída de texto. Independentemente do motivo, neste artigo, vamos mostrar como capitalizar uma string em Haskell de maneira simples e eficiente.

## Como Fazer

A função básica para capitalizar uma string em Haskell é `toUpper`, que pertence ao módulo `Data.Char`. Vamos ver um exemplo de como usá-la:

```
-- Importe o módulo Data.Char
import Data.Char

-- Defina uma string
texto = "hello world"

-- Capitalize a string usando a função toUpper
capitalizado = map toUpper texto
```

No código acima, importamos o módulo `Data.Char` para ter acesso à função `toUpper` e depois definimos uma string com a palavra "hello world". Em seguida, usamos a função `map` para aplicar a função `toUpper` a cada caractere da string, resultando em uma string com todas as letras em maiúsculo.

### Outros métodos

Além da função `toUpper`, existem outros métodos que podem ser usados para capitalizar uma string em Haskell:

- `toLower`: converte todos os caracteres para letras minúsculas.
- `toTitle`: converte o primeiro caractere de cada palavra para maiúscula.
- `toUpperFirst`: converte apenas o primeiro caractere para maiúscula.

Todas essas funções também pertencem ao módulo `Data.Char` e podem ser usadas da mesma maneira que a função `toUpper`.

## Deep Dive

Se olharmos para a documentação do módulo `Data.Char`, veremos que a função `toUpper` é definida da seguinte forma:

```
toUpper :: Char -> Char
```

Isso significa que a função recebe um caractere como entrada e retorna outro caractere como saída. Portanto, quando usamos `map toUpper` em uma string, na verdade estamos mapeando a função `toUpper` para cada caractere da string e, assim, obtendo uma string com os caracteres capitalizados.

No entanto, é importante ressaltar que a função `toUpper` não suporta caracteres especiais ou acentuados. Se você precisar capitalizar esse tipo de caractere, pode ser necessário usar uma biblioteca externa ou implementar sua própria função personalizada.

## Veja Também

- [Documentação do módulo `Data.Char`](https://hackage.haskell.org/package/base-4.14.1.0/docs/Data-Char.html)
- [Haskell para Iniciantes: Funções de Alta Ordem](https://blog.prettify.design/haskell-para-iniciantes-funcoes-de-alta-ordem/)