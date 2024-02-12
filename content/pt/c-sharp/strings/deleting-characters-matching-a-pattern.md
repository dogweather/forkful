---
title:                "Excluindo caracteres que correspondem a um padrão"
aliases:
- /pt/c-sharp/deleting-characters-matching-a-pattern/
date:                  2024-01-20T17:41:50.924143-07:00
model:                 gpt-4-1106-preview
simple_title:         "Excluindo caracteres que correspondem a um padrão"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/c-sharp/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## O Que & Por Quê?
Deletar caracteres que combinam com um padrão significa filtrar e remover partes indesejadas de uma string baseando-se em critérios específicos. Programadores fazem isso para limpar dados, validar entradas ou simplificar strings para processamento posterior.

## Como Fazer:
```C#
using System;
using System.Text.RegularExpressions;

public class Program
{
    public static void Main()
    {
        string textoOriginal = "C# é 4w350m3!";
        string padrao = @"\d";  // Deleta dígitos (0-9)
        
        string resultado = Regex.Replace(textoOriginal, padrao, "");
        Console.WriteLine(resultado);
    }
}
```
Saída:
```
C# é awesome!
```

## Mergulho Profundo
Historicamente, a manipulação de strings sempre foi uma habilidade crucial na maioria das linguagens de programação. Em C#, a classe `Regex`, parte do namespace `System.Text.RegularExpressions`, é a ferramenta padrão para trabalhos complexos com strings desde o .NET Framework 1.1. Uma alternativa à Regex seria usar métodos de string como `Replace` ou LINQ para deletar caracteres específicos, mas isso pode ser menos eficiente e mais complexo para padrões complicados. A Regex trabalha compilando uma expressão regular em um conjunto de instruções que são executadas contra a string de entrada, proporcionando uma forma poderosa e flexível de buscar e substituir padrões de texto.

## Veja Também:
- [Documentação oficial do .NET sobre a classe Regex](https://docs.microsoft.com/en-us/dotnet/api/system.text.regularexpressions.regex)
