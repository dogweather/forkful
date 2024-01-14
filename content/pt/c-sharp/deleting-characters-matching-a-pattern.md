---
title:                "C#: Excluir caracteres que correspondem a um padrão"
simple_title:         "Excluir caracteres que correspondem a um padrão"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/c-sharp/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Por que
Existem diversas razões pelas quais alguém pode querer excluir caracteres que correspondem a um determinado padrão em um programa em C#. Pode ser para lidar com entrada de dados inválida, para limpar e formatar strings antes de processá-las, ou simplesmente para remover elementos desnecessários de um texto.

## Como Fazer

Para deletar caracteres correspondentes a um padrão, podemos utilizar a função `Regex.Replace()` da biblioteca `System.Text.RegularExpressions`. Primeiramente, importe a biblioteca adicionando `using System.Text.RegularExpressions;` no início do seu código. Em seguida, vamos declarar uma string com o texto que desejamos processar e um padrão que queremos que seja correspondido, por exemplo:

```C#
string texto = "Esta é uma string com alguns caracteres inválidos #@$^&";
string padrao = "[^a-zA-Z0-9 ]+";
```

Agora, podemos utilizar a função `Regex.Replace()` passando como argumentos a string `texto`, o padrão `padrao` e uma string vazia `""` para indicar que queremos substituir os caracteres correspondentes por nada. O resultado será uma nova string com os caracteres inválidos removidos:

```C#
string resultado = Regex.Replace(texto, padrao, "");
Console.WriteLine(resultado);
// Imprime: "Esta é uma string com alguns caracteres inválidos "
```

## Deep Dive

O `\` é conhecido como escape character em C#, utilizado para indicar que o próximo caractere deve ser tratado de forma especial. Por exemplo, o `\n` é utilizado para representar uma quebra de linha. Porém, quando utilizado em conjuntos de caracteres em uma regex, ele tem o efeito de desabilitar os caracteres especiais, incluindo o `+` que representa uma ou mais ocorrências do padrão anteriormente especificado. Por isso, é necessário utilizar uma classe de caracteres negada (`[^...]`) para conseguir deletar os caracteres correspondentes ao padrão.

Existem diversas outras funções e métodos que podemos utilizar para lidar com padrões em C#, como a função `Regex.IsMatch()` para verificar se uma determinada string corresponde a um padrão, ou a classe `Regex.Match` para extrair trechos da string que correspondem a um padrão específico.

## Veja Também
- [Documentação do `Regex.Replace()` em MSDN](https://docs.microsoft.com/en-us/dotnet/api/system.text.regularexpressions.regex.replace?view=net-5.0)
- [Exemplos de utilização do `Regex` em C#](https://www.dotnetperls.com/regex)
- [Tutorial sobre expressões regulares em C#](https://www.youtube.com/watch?v=BBz4q3SQ4gA)