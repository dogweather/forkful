---
title:                "Encontrando o comprimento de uma string"
html_title:           "C#: Encontrando o comprimento de uma string"
simple_title:         "Encontrando o comprimento de uma string"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/c-sharp/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Por que encontrar o comprimento de uma string?

Encontrar o comprimento de uma string é uma tarefa comum em linguagens de programação, incluindo C#. Saber como fazer isso pode ser útil em muitos cenários, como validar o input do usuário, manipular dados em arquivos e muito mais.

## Como fazer isso?

Para encontrar o comprimento de uma string em C#, pode-se utilizar o método `Length`, que retorna o número de caracteres da string. Veja um exemplo abaixo:

```C#
string nome = "Maria";
Console.WriteLine(nome.Length); // output: 5
```

Este método é sensível a maiúsculas e minúsculas, ou seja, caracteres com acentos e letras maiúsculas serão contados como um caractere. Também é importante notar que o índice da string começa em 0, ou seja, o último caractere terá o índice igual ao comprimento da string -1.

Além disso, o método `Length` também pode ser utilizado em outras estruturas de dados, como arrays, listas e dicionários, retornando o número de elementos na estrutura.

## Deep Dive

Em C#, strings são imutáveis, o que significa que uma vez criada, seu conteúdo não pode ser alterado. Isso pode ser importante ao trabalhar com o comprimento de uma string, pois, como o método `Length` não faz nenhuma alteração, seu valor sempre permanecerá o mesmo.

Para garantir que uma string tenha um comprimento específico, pode-se utilizar o método `PadLeft` ou `PadRight`, que adicionam espaços vazios ou caracteres definidos para atingir o comprimento desejado.

Além disso, podemos acessar elementos individuais de uma string utilizando o operador `[]`, passando o índice desejado. Por exemplo:

```C#
string frase = "Olá, mundo!";
Console.WriteLine(frase[4]); // output: a
```

Também é possível utilizar o método `Substring` para obter uma parte específica de uma string, passando o índice de início e o comprimento desejado. Por exemplo:

```C#
string palavra = "programação";
Console.WriteLine(palavra.Substring(0,4)); // output: prog
```

Existem mais métodos e técnicas para trabalhar com o comprimento de uma string em C#, mas as principais já foram abordadas aqui. Agora, é hora de colocar esse conhecimento em prática!

## Veja também

-Como trabalhar com strings em C#: <https://www.w3schools.com/cs/cs_strings.asp>
-Documentação oficial do método `Length`: <https://docs.microsoft.com/en-us/dotnet/api/system.string.length>
-O guia completo de strings em C#: <https://docs.microsoft.com/en-us/dotnet/standard/base-types/strings>