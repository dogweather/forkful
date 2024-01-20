---
title:                "Usando expressões regulares"
html_title:           "Gleam: Usando expressões regulares"
simple_title:         "Usando expressões regulares"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/c-sharp/using-regular-expressions.md"
---

{{< edit_this_page >}}

## O que & Porquê?

Expressões regulares (Regex) são uma ferramenta poderosa e flexível para manipulação de strings. Os programadores usam Regex para encontrar, substituir e validar padrões em textos, o que ajuda a tornar o código mais compacto e eficiente.

## Como Fazer:

Em C#, usamos a classe `Regex` no namespace `System.Text.RegularExpressions`. Aqui está um exemplo simples que procura palavras que começam com "a":

```C#
using System;
using System.Text.RegularExpressions;

class Program
{
    static void Main(string[] args)
    {
        string frase = "O avião está no alto.";
        Match match = Regex.Match(frase, @"\ba\w*\b");

        if (match.Success)
        {
            Console.WriteLine("Encontrado: " + match.Value);
        }
    }
}
```
Esta execução retorna `Avião`.

É importante notar que os caracteres `\b` definem limites de palavras e o `\w*` significa qualquer caracter alfanumérico ou underscore. 

## Mergulho Profundo

Expressões regulares têm raízes históricas na teoria das linguagens formais e automação. Embora sejam extremamente úteis, elas têm suas limitações e pode haver alternativas dependendo do sistema ou da situação. 

Por exemplo, para tarefas de manipulação de strings muito complexas, pode ser melhor usar um analisador sintático. Além disso, a implementação de Regex pode variar ligeiramente entre diferentes linguagens de programação, portanto, é essencial verificar a documentação relevante.

Para otimizar a performance, C# compila as expressões regulares para o código Intermediate Language (IL) no primeiro uso, e as expressões compiladas são armazenadas em cache para uso futuro.

## Veja Também
- Documentação Microsoft Regex: [https://docs.microsoft.com/pt-br/dotnet/api/system.text.regularexpressions.regex?view=net-5.0](https://docs.microsoft.com/pt-br/dotnet/api/system.text.regularexpressions.regex?view=net-5.0)
- Regex Tutorial Interactivo: [https://regexone.com/](https://regexone.com/) 