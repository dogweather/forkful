---
title:                "Haskell: Transformando uma string em maiúsculas"
simple_title:         "Transformando uma string em maiúsculas"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/haskell/capitalizing-a-string.md"
---

{{< edit_this_page >}}

# Por que capitalizar uma string?

Muitos desenvolvedores podem se perguntar por que eles precisariam capitalizar uma string em seu código. A resposta é simples - às vezes, é necessário formatar as strings de maneira consistente, seja para apresentação visual ou para processamento de dados.

# Como fazer isso em Haskell?

Não se preocupe, capitalizar uma string em Haskell é fácil! Basta seguir os seguintes passos:

### Passo 1: Importar o módulo "Data.Char"
Primeiro, você precisa importar o módulo "Data.Char" para ter acesso às funções de manipulação de caracteres.

```Haskell
import Data.Char
```

### Passo 2: Usando a função "toUpper"
Agora, você pode usar a função "toUpper" para capitalizar uma string. Esta função leva um caractere como parâmetro e retorna o mesmo caractere, mas em maiúsculo.

```Haskell
toUpper 'a' -- retorna 'A'
```

### Passo 3: Usando a função "map"
Para capitalizar uma string inteira, você pode usar a função "map" em conjunto com a função "toUpper". A função "map" aplicará a função "toUpper" em cada caractere da string.

```Haskell
map toUpper "hello" -- retorna "HELLO"
```

E voilà, você acabou de capitalizar uma string em Haskell!

# Aprofundando na capitalização de strings

Se você quiser entender melhor a função "toUpper", pode dar uma olhada na sua implementação em Haskell:

```Haskell
toUpper :: Char -> Char
toUpper c
    | ord c >= 97 && ord c <= 122 = chr (ord c - 32)
    | otherwise = c
```

Esta função primeiro verifica se o caractere fornecido está dentro dos limites ASCII de letras minúsculas (97 a 122). Se estiver, ele converte o caractere para seu equivalente em maiúsculo (subtraindo 32 do seu código ASCII). Caso contrário, retorna o caractere original.

# Veja também
- [Documentação do módulo "Data.Char"](https://hackage.haskell.org/package/base-4.14.0.0/docs/Data-Char.html)
- [Tutorial de Haskell em português](https://haskell.tailorfontela.com.br/)