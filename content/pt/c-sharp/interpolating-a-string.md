---
title:                "Interpolando uma string"
html_title:           "C#: Interpolando uma string"
simple_title:         "Interpolando uma string"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/c-sharp/interpolating-a-string.md"
---

{{< edit_this_page >}}

## O que e por que?

Interpolar uma string é uma maneira mais fácil e eficiente de combinar strings e variáveis em uma única cadeia de caracteres. Os programadores usam a interpolação de string para tornar o código mais legível e manter a formatação correta em suas saídas.

## Como fazer:

```
string name = "Maria";
int age = 25;
Console.WriteLine($"Olá, meu nome é {name} e tenho {idade} anos."); 
```

O resultado será: 
```
Olá, meu nome é Maria e tenho 25 anos.
```

## Profundando:

A interpolação de string foi introduzida no C# 6 e se tornou uma maneira popular de combinar strings e variáveis. Anteriormente, os programadores usavam a concatenação manual ou métodos de formatação como `String.Format()` para realizar essa tarefa. No entanto, a interpolação de string torna o código mais legível e reduz a chance de erros de formatação.

## Veja também:

- [Documentação oficial do C# sobre interpolação de string](https://docs.microsoft.com/pt-br/dotnet/csharp/language-reference/tokens/interpolated)
- [Artigo sobre interpolação de string no DevMedia](https://www.devmedia.com.br/csharp-string-interpolation-interpolacao-de-strings/32916)
- [Vídeo explicativo sobre interpolação de string no canal Programador BR](https://www.youtube.com/watch?v=4c6FfYQnucM)