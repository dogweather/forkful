---
title:                "Convertendo uma string para minúsculas"
date:                  2024-01-20T17:38:25.736304-07:00
model:                 gpt-4-1106-preview
simple_title:         "Convertendo uma string para minúsculas"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/c-sharp/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## What & Why? (O Que & Porquê?)
Converter uma string para letras minúsculas é o processo de transformar todos os caracteres alfabéticos de uma cadeia de texto em suas equivalentes em caixa baixa. Programadores fazem isso para padronizar dados, facilitar comparações de strings insensíveis a maiúsculas e minúsculas e melhorar a consistência do texto.

## How to (Como Fazer):
Em C#, você pode converter uma string para minúsculas usando o método `.ToLower()` ou `.ToLowerInvariant()`. O primeiro respeita as configurações de cultura local, enquanto o segundo aplica a cultura invariante.

```C#
string original = "Olá, MUNDO!";
string emMinusculas = original.ToLower();
string emMinusculasInvariante = original.ToLowerInvariant();

Console.WriteLine(emMinusculas); // saída: "olá, mundo!"
Console.WriteLine(emMinusculasInvariante); // saída: "olá, mundo!"
```

## Deep Dive (Mergulho Profundo)
Historicamente, converter para minúsculas ajudou a normalizar texto em casos como armazenamento de dados e busca textual. Linguagens de programação antigas já apresentavam funções para essa transformação, refletindo a necessidade comum.

Além dos métodos `.ToLower()` e `.ToLowerInvariant()`, existem alternativas como `String.Compare()` e `String.Equals()` com comparação insensível a caixa para não ter que modificar a string original. Ao implementar `.ToLower()`, o comportamento pode variar com a cultura. Por exemplo, na cultura turca, a letra 'i' maiúscula tem uma versão minúscula diferente ('ı') quando comparada ao inglês.

Considere:
- Uso de `.ToLowerInvariant()` para consistência além das configurações locais do usuário.
- Desempenho pode ser impactado ao transformar grandes volumes de texto repetidamente.

## See Also (Veja Também)
- Documentação oficial do .NET sobre String.ToLower: [docs.microsoft](https://docs.microsoft.com/en-us/dotnet/api/system.string.tolower)
- Documentação oficial do .NET sobre String.ToLowerInvariant: [docs.microsoft](https://docs.microsoft.com/en-us/dotnet/api/system.string.tolowerinvariant)
- CompareInfo class para comparações complexas de string: [docs.microsoft](https://docs.microsoft.com/en-us/dotnet/api/system.globalization.compareinfo)
- CultureInfo class e influência cultural na manipulação de strings: [docs.microsoft](https://docs.microsoft.com/en-us/dotnet/api/system.globalization.cultureinfo)