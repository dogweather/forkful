---
title:                "Excluindo caracteres que correspondem a um padrão"
html_title:           "C#: Excluindo caracteres que correspondem a um padrão"
simple_title:         "Excluindo caracteres que correspondem a um padrão"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/c-sharp/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## O que e por que?

Deletar caracteres que correspondem a um padrão é uma tarefa comum para os programadores. Isso envolve remover caracteres específicos de uma determinada string, seguindo um padrão predefinido. Os programadores fazem isso para limpar dados, formatar strings ou alterar o formato de uma determinada string.

## Como fazer:

Segue um exemplo de código em C# para deletar caracteres que correspondem a um padrão em uma string:

```C#
string texto = "Este é um exemplo de string";
string textoModificado = texto.Replace("e", "");

//O resultado será "Est é um xmplo d string"
```

## Deep Dive:

Deletar caracteres que correspondem a um padrão se tornou uma tarefa essencial para os programadores com o avanço da tecnologia. No passado, era feito de forma manual ou com a ajuda de softwares específicos. Hoje em dia, com o uso de linguagens de programação como o C#, essa tarefa se tornou muito mais simples e eficiente.

Existem várias formas de deletar caracteres que correspondem a um padrão em uma string. Além do método "Replace" mostrado no exemplo acima, também é possível usar expressões regulares ou funções específicas da linguagem de programação. Cada método tem suas vantagens e desvantagens, e cabe ao programador escolher o mais adequado para cada situação.

A implementação dessa tarefa pode variar de acordo com a linguagem de programação utilizada. Por exemplo, em C#, é necessário utilizar o método "Replace" para substituir os caracteres que correspondem ao padrão, enquanto em outras linguagens pode ser necessário utilizar um loop para percorrer a string e deletar manualmente os caracteres desejados.

## Veja também:

- [Microsoft Docs - Como: Substituir texto em uma string](https://docs.microsoft.com/pt-br/dotnet/csharp/how-to/modify-string-contents)
- [DevMedia - Expressões Regulares com C#](https://www.devmedia.com.br/expressoes-regulares-com-csharp/2258)