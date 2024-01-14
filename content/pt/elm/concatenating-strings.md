---
title:                "Elm: Concatenando strings"
programming_language: "Elm"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/elm/concatenating-strings.md"
---

{{< edit_this_page >}}

## Por que 

Se você é um desenvolvedor iniciante ou já tem alguma experiência em programação, é importante saber como concatenar strings em seu código. Essa função é muito útil para juntar diferentes peças de informações e criar uma única string.

## Como Fazer 

Para concatenar strings no Elm, você pode usar o operador `++`. Esse operador pega duas strings e as coloca uma ao lado da outra, criando uma nova string combinada. Veja um exemplo abaixo:

```Elm 
nome = "Ana"
sobrenome = "Silva"

nomeCompleto = nome ++ " " ++ sobrenome

-- output: "Ana Silva"
```

Você também pode usar o operador `++` para concatenar strings com variáveis, números ou até mesmo outras strings concatenadas. Veja alguns exemplos a seguir:

```Elm
idade = 25
mensagem = "Minha idade é: "

mensagemCompleta = mensagem ++ (toString idade) 

-- output: "Minha idade é: 25"
```

```Elm
mensagem = "Eu quero ter " ++ (toString 100) ++ " likes nessa postagem."

-- output: "Eu quero ter 100 likes nessa postagem."
```

```Elm 
primeiraParte = "Hello"
segundaParte = "world"

mensagem = primeiraParte ++ ", " ++ "secondParte ++ "!"

-- output: "Hello, world!"
```

## Aprofundando 

É importante notar que o operador `++` sempre retorna uma nova string e não modifica as strings originais. Além disso, podemos usar o operador `++` com mais de duas strings, criando uma cadeia de concatenação. Veja um exemplo abaixo:

```Elm 
primeiraParte = "Eu gosto de "
segundaParte = "programar em "
terceiraParte = "Elm"

mensagem = primeiraParte ++ segundaParte ++ terceiraParte 

-- output: "Eu gosto de programar em Elm" 
```

## Veja também 

Aqui estão alguns recursos adicionais para ajudá-lo a aprender mais sobre como concatenar strings em Elm:

- [Documentação oficial do operador ++](https://package.elm-lang.org/packages/elm/core/latest/String#++)
- [Tutorial sobre strings em Elm](https://www.tutorialspoint.com/elm/elm_strings.htm)
- [Outros operadores úteis em Elm](https://medium.com/@aleiadeveloper/10-must-know-elm-operators-part-1-27a3ef9d8c0e)