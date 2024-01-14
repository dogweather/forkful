---
title:                "C#: Utilizando expressões regulares"
simple_title:         "Utilizando expressões regulares"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/c-sharp/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Por Que

Se você já programou em C# ou qualquer outra linguagem, provavelmente já se deparou com a necessidade de pesquisar e manipular cadeias de caracteres. É aí que as expressões regulares entram como uma ferramenta poderosa para ajudar a identificar e extrair padrões em uma string. Usar expressões regulares pode economizar tempo e tornar o código mais eficiente.

## Como Usar

Para usar expressões regulares em C#, você pode simplesmente importar o namespace `System.Text.RegularExpressions` e começar a escrever seus padrões de correspondência. Aqui está um exemplo para validar um endereço de e-mail:

```
Regex regex = new Regex(@"^[\w\.+-]+@[a-z]+\.[a-z]{2,}$");

string email = "exemplo@dominio.com";

if (regex.IsMatch(email))
{
    Console.WriteLine("Endereço de e-mail válido!");
}
```

Neste exemplo, usamos a classe `Regex` para criar um objeto que representa o nosso padrão de correspondência. Em seguida, usamos o método `IsMatch()` para verificar se a string atende ao padrão. Isso é apenas um exemplo básico, mas existem muitas regras e recursos que podem ser usados em expressões regulares para atender às suas necessidades específicas.

## Deep Dive

O poder das expressões regulares não se restringe apenas ao básico. Existem muitos recursos avançados disponíveis para torná-las ainda mais poderosas. Alguns desses recursos incluem:

- O uso de quantificadores como `*` (zero ou mais ocorrências), `+` (uma ou mais ocorrências) e `{n,m}` (no mínimo n e no máximo m ocorrências).
- Captura de grupos para extrair partes específicas da string correspondente.
- Uso de metacaracteres como `\d` (qualquer dígito), `\w` (qualquer caractere alfanumérico) e `\s` (qualquer espaço em branco).
- A capacidade de substituir padrões de correspondência por outra string.

Com um pouco de prática, você pode usar esses recursos para criar expressões regulares poderosas que ajudam a resolver problemas de formatação, validação e extração de dados em suas aplicações.

## Veja Também

- Documentação oficial da classe Regex em C#: https://docs.microsoft.com/pt-br/dotnet/api/system.text.regularexpressions.regex?view=net-5.0
- Tutorial interativo sobre expressões regulares em C#: https://regexone.com/references/csharp