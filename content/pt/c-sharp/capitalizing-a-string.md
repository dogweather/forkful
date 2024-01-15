---
title:                "Capitalizando uma string"
html_title:           "C#: Capitalizando uma string"
simple_title:         "Capitalizando uma string"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/c-sharp/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Por que

Quando estamos trabalhando com strings em C#, pode ser necessário capitalizar uma ou mais palavras, seja para deixar o texto mais legível ou para fins de formatação. Felizmente, o C# possui uma função simples para realizar essa tarefa.

## Como Fazer

Para capitalizar uma string em C#, utilizamos o método `ToUpper()` da classe `string`. Veja o exemplo abaixo:

```C#
string texto = "exemplo de texto";
string textoMaiusculo = texto.ToUpper();

Console.WriteLine(textoMaiusculo); // Exibe "EXEMPLO DE TEXTO"
```

Como podemos ver, o método `ToUpper()` transforma todas as letras do texto em maiúsculas. Porém, se quisermos apenas capitalizar a primeira letra, podemos usar o método `ToTitleCase()` da classe `TextInfo` da seguinte forma:

```C#
string texto = "exemplo de texto";
TextInfo textInfo = new CultureInfo("pt-BR").TextInfo;
string textoTitulo = textInfo.ToTitleCase(texto);

Console.WriteLine(textoTitulo); // Exibe "Exemplo De Texto"
```

Além disso, é possível especificar o idioma a ser utilizado no método `ToTitleCase()`, garantindo uma capitalização de acordo com as regras de cada língua.

## Mergulho Profundo

O método `ToUpper()` e `ToTitleCase()` são chamados de "métodos de extensão", o que significa que eles são aplicados diretamente em um objeto `string`. Isso permite que o código fique mais limpo e legível. Além disso, o C# possui outras funções úteis para trabalhar com strings, como `ToLower()`, `Trim()` e `Replace()`, que auxiliam na manipulação e formatação de textos.

## Veja Também

- [Documentação Oficial do C#](https://docs.microsoft.com/pt-br/dotnet/csharp/)
- [Lista de Culturas Suportadas pelo C#](https://docs.microsoft.com/pt-br/dotnet/api/system.globalization.cultureinfo?view=netcore-3.1)
- [Tutorial de C# para Iniciantes](https://www.c-sharpcorner.com/article/c-sharp-tutorial-for-beginners/)