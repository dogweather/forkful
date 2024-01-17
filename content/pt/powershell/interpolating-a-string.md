---
title:                "Interpolando uma string."
html_title:           "PowerShell: Interpolando uma string."
simple_title:         "Interpolando uma string."
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/powershell/interpolating-a-string.md"
---

{{< edit_this_page >}}

## O que e Por que?

Interpolação de string no PowerShell é a capacidade de inserir valores ou variáveis em uma string para criar uma mensagem personalizada dinamicamente. Isso é frequentemente usado pelos programadores para criar saídas mais claras e legíveis, em vez de ter que concatenar ou inserir manualmente os valores em uma string.

## Como fazer:

```PowerShell
# Definindo uma variável com o valor a ser inserido na string
$nome = "Ana"

# Interpolação de uma string com o valor da variável
"Olá $nome, bem-vinda ao meu artigo sobre PowerShell!"

# Saída: Olá Ana, bem-vinda ao meu artigo sobre PowerShell!
```

Você também pode inserir valores de variáveis ou expressões dentro de strings usando o caractere de cifrão seguido por chaves `{}`. Isso é útil quando você precisa usar um operador no valor da variável, como neste exemplo usando uma lista:

```PowerShell
# Definindo uma variável com uma lista
$nomes = @("Ana", "João", "Maria")

# Interpolação de uma string com o valor da variável e o operador de seleção (-join)
"Alunos: $($nomes -join ', ')"

# Saída: Alunos: Ana, João, Maria
```

## Mergulho profundo:

A interpolação de string é baseada na linguagem C# e foi introduzida no PowerShell 3.0. Antes disso, os programadores precisavam usar o método `StringBuilder` para criar strings interpoladas. Alguns outros métodos alternativos para realizar interpolação de string no PowerShell são o `Format-String`, `Out-String` e `String-Expand`.

A implementação da interpolação de string no PowerShell é feita usando o método `String.Format()` da linguagem C# e é executada pelo mecanismo de linguagem do PowerShell. No entanto, ao contrário do C#, o PowerShell usa a ordem de execução dos valores na string em vez de índices de formatação.

## Veja também:

- [Microsoft Docs: Interpolação de string no PowerShell](https://docs.microsoft.com/pt-br/powershell/module/microsoft.powershell.core/about/about_quoting_rules?view=powershell-5.1)
- [Interpolação de string no C#](https://docs.microsoft.com/pt-br/dotnet/csharp/language-reference/tokens/interpolated)
- [Alternativas para Interpolação de string no PowerShell](https://powershellexplained.com/2015-06-21-powershell-what-is-interpolation/)