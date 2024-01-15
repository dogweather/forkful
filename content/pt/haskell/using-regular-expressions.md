---
title:                "Utilizando expressões regulares"
html_title:           "Haskell: Utilizando expressões regulares"
simple_title:         "Utilizando expressões regulares"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/haskell/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Por que usar expressões regulares?

Se você é um programador iniciante ou experiente, provavelmente já se deparou com a necessidade de buscar padrões em textos e strings. Talvez você precise encontrar e-mails em um documento ou validar senhas em um formulário. É aí que entram as expressões regulares. Elas são um recurso poderoso e versátil que permite buscar e manipular padrões de strings de forma eficiente. Em resumo, usar expressões regulares pode facilitar a vida do programador e tornar o código mais compacto e legível.

## Como fazer isso em Haskell?

Para usar expressões regulares em Haskell, primeiro precisamos importar o módulo `Text.Regex.Posix`. Em seguida, vamos criar um pequeno exemplo que busca números em um texto. Confira o código abaixo:

```Haskell
import Text.Regex.Posix

-- texto que será analisado
texto = "Eu tenho 3 maçãs e 5 laranjas"

-- expressão regular que busca números
regex = "[0-9]+"

-- função que aplica a expressão regular ao texto e retorna uma lista de números
buscarNumeros :: String -> [String]
buscarNumeros str = str =~ regex :: [String]

main = do
  -- mostra o texto original
  putStrLn $ "Texto original: " ++ texto
  
  -- mostra a lista de números encontrados
  putStrLn $ "Números encontrados: " ++ show (buscarNumeros texto)
```

A saída desse código será:

```
Texto original: Eu tenho 3 maçãs e 5 laranjas
Números encontrados: ["3","5"]
```

Vamos analisar o código passo a passo:

1. Importamos o módulo `Text.Regex.Posix`.
2. Criamos uma variável `texto` com o texto a ser analisado.
3. Definimos nossa expressão regular na variável `regex`. Nesse caso, queremos buscar sequências de dígitos, então usamos `[0-9]+`.
4. Criamos uma função `buscarNumeros` que recebe uma string e retorna uma lista de strings (que serão os números encontrados).
5. Dentro da função, usamos o operador `=~` para aplicar a expressão regular ao texto. O resultado é uma lista de strings, então usamos `:: [String]` para especificar o tipo de dados.
6. No `main`, mostramos o texto original e a lista de números encontrados.

Além do operador `=~`, também temos o operador `=~%` que só retorna o primeiro match da expressão regular. E se quisermos substituir esses números por outra coisa? Basta usar o operador `subRegex`. Confira um exemplo:

```Haskell
import Text.Regex.Posix

-- texto original
texto = "Eu tenho 3 maçãs e 5 laranjas"

-- expressão regular que busca números
regex = "[0-9]+"

-- função que substitui os números por asteriscos
substituirPorAsteriscos :: String -> String
substituirPorAsteriscos str = subRegex (str =~ regex) "\\*" str

main = do
  -- mostra o texto original
  putStrLn $ "Texto original: " ++ texto
  
  -- mostra o texto com os números substituídos por asteriscos
  putStrLn $ "Texto com números substituídos: " ++ substituirPorAsteriscos texto
```

A saída será:

```
Texto original: Eu tenho 3 maçãs e 5 laranjas
Texto com números substituídos: Eu tenho * maçãs e * laranjas
```

## Mergulhando mais fundo

Esses foram apenas exemplos simples de como usar expressões regulares em Haskell. Para explorar mais funcionalidades e possibilidades, recomendo dar uma olhada na documentação oficial do módulo `Text.Regex.Posix` e também na biblioteca `regex-posix` no Hackage. Além disso, é sempre bom praticar e experimentar em seus próprios projetos para se familiarizar com o uso das expressões regulares.

## Veja também

* Documentação oficial do módulo `Text.Regex.Posix`: https://hackage.haskell.org/package/regex-posix-0.95.2/docs/Text-Regex-Posix.html
* Biblioteca `regex-posix` no Hackage: https://hackage.haskell.org/package/regex-posix