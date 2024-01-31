---
title:                "Convertendo uma string para minúsculas"
date:                  2024-01-20T17:39:31.716543-07:00
model:                 gpt-4-1106-preview
simple_title:         "Convertendo uma string para minúsculas"

category:             "PowerShell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/powershell/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## O Que é & Porquê?
Converter uma string para minúsculas é o processo de transformar todos os caracteres alfabéticos numa string de forma a que todos eles sejam minúsculos. Programadores fazem isso para padronizar dados, facilitar comparações de texto e garantir consistência em operações que são sensíveis a maiúsculas e minúsculas.

## Como Fazer:
Para converter uma string para minúsculas no PowerShell, você pode usar o método `.ToLower()`. Veja como é simples:

```PowerShell
$stringOriginal = "Olá, Mundo!"
$stringMinúsculas = $stringOriginal.ToLower()
$stringMinúsculas
```

Saída esperada:
```
olá, mundo!
```

Você também pode utilizar o cmdlet `ToLowerInvariant()` se precisar garantir uniformidade independentemente das configurações regionais do sistema:

```PowerShell
$stringOriginal = "FELIZ ANIVERSÁRIO!"
$stringMinúsculasInvariantes = $stringOriginal.ToLowerInvariant()
$stringMinúsculasInvariantes
```

Saída esperada:
```
feliz aniversário!
```

## Mergulho Profundo
Historicamente, a necessidade de converter strings para minúsculas surgiu da diversidade de formas como os computadores manipulam texto. Apesar de à primeira vista parecer uma tarefa simples, a conversão envolve considerar conjuntos de caracteres, codificações e até diferenças culturais ou idiomáticas de como as letras são representadas em diferentes línguas.

Alternativamente à função nativa do PowerShell, outras linguagens de programação oferecem funções semelhantes, como `toLowerCase()` em JavaScript ou `lower()` em Python. Mesmo dentro do PowerShell, você poderia usar métodos .NET, tais como `[string]::ToLower()`, que por baixo dos panos faz essencialmente o mesmo que `.ToLower()` do PowerShell.

Compreender as implicações da implementação é crucial. Por exemplo, o método `ToLowerInvariant()` ignora a cultura do sistema atual e utiliza a cultura invariante, o que é útil para dados que serão armazenados e comparados de forma consistente, independentemente da localidade.

## Veja Também
- [Documentação oficial do PowerShell](https://docs.microsoft.com/pt-br/powershell/)
- [.NET String Methods](https://docs.microsoft.com/dotnet/api/system.string)
- [Informações sobre Cultura Invariante](https://docs.microsoft.com/dotnet/api/system.globalization.cultureinfo.invariantculture)
