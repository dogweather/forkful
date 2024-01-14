---
title:    "Elm: Convertendo uma string para letras minúsculas"
keywords: ["Elm"]
---

{{< edit_this_page >}}

## Por que converter uma string para letras minúsculas?

Você pode pensar que isso é algo simples e trivial, mas a conversão de uma string para letras minúsculas pode ser muito útil em certas situações. Por exemplo, quando você está trabalhando com dados de entrada fornecidos pelo usuário, faz sentido padronizar tudo em letras minúsculas para evitar erros ao comparar strings.

## Como fazer isso em Elm?

Em Elm, a conversão de uma string para letras minúsculas é feita através da função `String.toLower`. Veja abaixo um exemplo simples:

```Elm
String.toLower "ELM PROGRAMMING" -- Output: "elm programming"
```

Se a string já estiver em letras minúsculas, a função apenas retorna a própria string. Além disso, a função `String.toLower` não altera a string original, mas retorna uma nova string com as letras em minúsculo.

Agora, vamos ver como isso pode ser útil no contexto de um formulário de login:

```Elm
username = "USUÁRIO"
password = "Tester123"

if (username == (String.toLower "Usuário")) && (password == "tester123") then
    loginSuccessful
else
    loginFailed
```

Neste caso, a string de entrada `username` é convertida para letras minúsculas antes de ser comparada com a string "usuário". Isso garante que, independentemente de como o usuário digitou seu nome de usuário, o login será bem-sucedido desde que a senha esteja correta.

## Mais detalhes sobre a conversão de strings em Elm

Em Elm, a conversão de uma string para letras minúsculas é baseada no Unicode, o que significa que também funciona para caracteres acentuados e símbolos especiais. Além disso, a função `String.toLower` pode ser usada em conjunto com outras funções da biblioteca `String`, como por exemplo `String.toUpper`, para manipular e formatar strings de diversas formas.

## Veja também
- [Documentação oficial do Elm sobre conversão de strings](https://package.elm-lang.org/packages/elm/core/latest/String)
- [Elm String Cheat Sheet](https://devhints.io/elm-strings)
- [Tutorial sobre strings em Elm](https://www.learnhowtoprogram.com/intermediate-programming-concepts/functional-programming-in-javascript/strings)

Esperamos que este artigo tenha sido útil para você entender um pouco mais sobre como trabalhar com strings em Elm. Continue explorando a documentação oficial e praticando com exemplos para aperfeiçoar suas habilidades de programação nessa linguagem funcional maravilhosa!