---
title:    "Elm: Juntando cadeias de caracteres"
keywords: ["Elm"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/elm/concatenating-strings.md"
---

{{< edit_this_page >}}

## Por que

Concatenar strings é uma tarefa comum na programação, especialmente quando se trata de manipular dados e criar mensagens personalizadas. No Elm, isso pode ser feito de forma eficiente e concisa usando suas poderosas funções de concatenação de strings.

## Como Fazer

Existem duas maneiras de concatenar strings no Elm: usando a função `++` ou o operador de concatenação `++=`. Ambos tomam duas strings como argumentos e retornam uma nova string que é a combinação das duas. Veja um exemplo abaixo:

```elm
nomeCompleto : String
nomeCompleto = "João " ++ "Silva"

mensagem : String
mensagem = "Bem-vindo " ++= nomeCompleto ++ ", seu cadastro foi concluído com sucesso!"

main = text mensagem
```

Ao rodar este código, a seguinte saída será exibida:

```
Bem-vindo João Silva, seu cadastro foi concluído com sucesso!
```

Além disso, é possível concatenar mais de duas strings, simplesmente encadeando vários operadores `++` ou `++=`. Por exemplo:

```elm
mensagemPersonalizada : String
mensagemPersonalizada = "Olá " ++= nomeCompleto ++ ", hoje é " ++= data ++ =", obrigado por se juntar a nós!"
```

## Profundidade

Em Elm, as strings são representadas como listas de caracteres, o que significa que as funções de lista também podem ser usadas para manipular strings. Isso inclui a função `append` que pode ser usada para adicionar um caractere ao final de uma string. Veja um exemplo abaixo:

```elm
nome : String
nome = "Maria"

saudacao : String
saudacao = "Olá " ++ String.fromList (List.append [Char.toCode "!" ++ nome
```

Neste exemplo, usamos a função `String.fromList` para converter a lista resultante da concatenação para uma string novamente.

## Veja Também

- Documentação oficial do Elm sobre concatenação de strings: https://guide.elm-lang.org/strings/concatenation.html
- Exemplos de concatenação de strings em Elm: https://ellie-app.com/new