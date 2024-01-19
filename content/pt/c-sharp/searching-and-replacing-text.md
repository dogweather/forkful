---
title:                "Pesquisando e substituindo texto"
html_title:           "Bash: Pesquisando e substituindo texto"
simple_title:         "Pesquisando e substituindo texto"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/c-sharp/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## O Que & Por Quê?

A busca e substituição de texto são comuns em muitas tarefas de programação. Por exemplo, podemos querer atualizar um nome de variável ou modificar um valor na codificação. Fazemos isso para tornar o código mais preciso, atualizado ou eficiente.

## Como fazer:

Vamos ver como fazemos isso em C#, utilizando o método 'Replace'.

```C#
string textoInicial = "Olá, mundo!";
string textoAlterado = textoInicial.Replace("mundo", "Brasil");
Console.WriteLine(textoAlterado);  // Saída: "Olá, Brasil!"
```
Isso substitui todas as ocorrências de "mundo" por "Brasil" no texto inicial.

## Mergulho profundo:

Primeiro, um pouco da história. Em programação, a necessidade de busca e substituição apareceu quase tão logo as primeiras linguagens foram criadas. Linguagens como sed (stream editor) em Unix são projetados principalmente para essa tarefa!

C# faz isto através da classe `String` e do seu método `Replace()`. Os métodos são altamente otimizados para operações em larga escala.

Alternativas? Podemos usar expressões regulares se precisarmos de mais poder - substituir baseado em padrões de texto, não somente valores exatos. Veja:

```C#
string texto = "O meu número é: 123-456-7890";
string substituicao = Regex.Replace(texto, @"\d", "*");
Console.WriteLine(substituicao);  // Saída: "O meu número é: ***-***-****"
```
Aqui, substituímos todos os dígitos por asteriscos!

Na implementação real, o método 'Replace' primeiro busca pelo texto à ser substituído. Quando ele o encontra, aloca uma nova string de tamanho apropriado e começa a copiar os caracteres da string original para a nova. Quando atinge o texto a ser substituído, ele grava o novo texto e em seguida continua copiando.

## Ver também:

- [String.Replace Método ](https://docs.microsoft.com/pt-br/dotnet/api/system.string.replace?view=netcore-3.1)
- [Expressões regulares em C#](https://docs.microsoft.com/pt-br/dotnet/standard/base-types/regular-expressions)
- [Metodo Regex.Replace ](https://docs.microsoft.com/pt-br/dotnet/api/system.text.regularexpressions.regex.replace?view=netcore-3.1)