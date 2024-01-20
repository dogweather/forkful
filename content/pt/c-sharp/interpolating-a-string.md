---
title:                "Interpolando uma string"
html_title:           "Java: Interpolando uma string"
simple_title:         "Interpolando uma string"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/c-sharp/interpolating-a-string.md"
---

{{< edit_this_page >}}

# Interpolação de String em C#

## 1. O Que e Por Que?
A interpolação de string é um método para inserir valores de variáveis ​​diretamente em uma string em tempo de execução. Os programadores utilizam isso para construir strings de maneira mais limpa e fácil de ler.

## 2. Como Fazer:

Eis um exemplo básico de como interpolar uma string:

```C#
string nome = "João";
int idade = 30;
var str = $"O nome é {nome} e a idade é {idade}.";
```

A saída resultante será:

```
O nome é João e a idade é 30.
```

Você também pode formatar valores numéricos como no exemplo a seguir:

```C#
double valor = 1234.56789;
var str = $"O valor formatado é {valor:F2}.";
```

A saída será:

```
O valor formatado é 1234.57.
```

## 3. Análise Profunda
A interpolação de string foi introduzida no C# 6.0, tornando seu código mais enxuto e mais fácil de manter. Antes disso, os programadores usavam a concatenação de string ou string.Format() que poderiam ser um pouco complicados e propensos a erros.

As alternativas à interpolação de string incluem o uso de concatenação (+) ou do método string.Format(). No entanto, a interpolação de string aumenta a legibilidade, reduzindo erros.

Durante a compilação, as strings interpoladas são convertidas em invocações de `String.Format()`. Isso significa que as strings interpoladas seguem as mesmas regras de formatação que `string.Format()`.

## 4. Veja Também
Para mais informações sobre a interpolação de string, consulte os links a seguir:

- [Interpolação de String (C# Guia de Programação)](https://docs.microsoft.com/pt-br/dotnet/csharp/language-reference/tokens/interpolated)