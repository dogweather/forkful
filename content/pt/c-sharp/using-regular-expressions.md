---
title:                "Utilizando expressões regulares"
html_title:           "C#: Utilizando expressões regulares"
simple_title:         "Utilizando expressões regulares"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/c-sharp/using-regular-expressions.md"
---

{{< edit_this_page >}}

O que é e por que usar Expressões Regulares

Expressões Regulares são padrões de texto usados para buscar, extrair e substituir informações em uma string. Programadores usam expressões regulares para realizar tarefas como validação de formato de entrada de dados, busca e filtro de informações em um texto e substituição de caracteres específicos.

Como fazer:
```C#
using System;
using System.Text.RegularExpressions;

class Program {
    static void Main() {
        // Busca por um padrão específico em uma string
        string texto = "Olá meu nome é João";
        string padrao = "João";
        Regex regex = new Regex(padrao);
        Console.WriteLine(regex.IsMatch(texto)); //true

        // Substitui um padrão em uma string
        string novoTexto = Regex.Replace(texto, "Olá", "Oi");
        Console.WriteLine(novoTexto); //Oi meu nome é João
    }
}
```

Mergulho profundo:

Expressões Regulares têm suas raízes na teoria matemática dos autômatos finitos e suas aplicações práticas remontam à década de 1940. Elas são amplamente usadas em diferentes linguagens de programação e podem variar em sua sintaxe e recursos. Algumas alternativas para expressões regulares incluem o uso de funções de string específicas da linguagem, mas expressões regulares são frequentemente a maneira mais concisa e abrangente para manipular e extrair informações de uma string. A implementação de expressões regulares em C# é feita através do namespace System.Text.RegularExpressions.

Veja também:
- Documentação oficial do C#: https://docs.microsoft.com/pt-br/dotnet/csharp/
- Tutorial de expressões regulares em C#: https://www.tutorialspoint.com/csharp/csharp_regular_expressions.htm