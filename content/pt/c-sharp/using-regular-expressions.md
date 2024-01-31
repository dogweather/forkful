---
title:                "Utilizando expressões regulares"
date:                  2024-01-19
html_title:           "Bash: Utilizando expressões regulares"
simple_title:         "Utilizando expressões regulares"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/c-sharp/using-regular-expressions.md"
---

{{< edit_this_page >}}

## O Que & Porquê?

Expressões regulares (regex) vasculham texto para padrões específicos — útil para validações, busca, e substituições automáticas. Programadores usam para poupar tempo e evitar erros repetitivos.

## Como Fazer:

Código básico para validar um email:

```C#
using System;
using System.Text.RegularExpressions;

class Program
{
    static void Main()
    {
        string email = "usuario@example.com";
        bool isValid = Regex.IsMatch(email, @"^[^@\s]+@[^@\s]+\.[^@\s]+$");

        Console.WriteLine(isValid ? "Email válido." : "Email inválido.");
    }
}
```

Saída esperada:

```
Email válido.
```

Substituir espaços por traços em uma string:

```C#
using System;
using System.Text.RegularExpressions;

class Program
{
    static void Main()
    {
        string texto = "Texto com espaços";
        string substituido = Regex.Replace(texto, @"\s+", "-");

        Console.WriteLine(substituido);
    }
}
```

Saída esperada:

```
Texto-com-espaços
```

## Visão Detalhada:

Regex vem da década de 1950, com uso em teoria da computação e linguística formal. Alternativas incluem parsing manual ou bibliotecas especializadas de manipulação de strings; no entanto, regex oferece flexibilidade inigualável. No C#, a implementação de regex é feita através do namespace `System.Text.RegularExpressions`, que compila expressões regulares para um formato intermediário rápido e eficiente.

## Ver Também:

- [Documentação oficial de expressões regulares em C#](https://docs.microsoft.com/pt-br/dotnet/standard/base-types/regular-expressions)
- [Tutorial interativo de regex](https://regexone.com/)
- [Ferramenta online para testar regex](https://regexr.com/)
