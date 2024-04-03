---
date: 2024-01-20 17:38:25.736304-07:00
description: "How to (Como Fazer): Em C#, voc\xEA pode converter uma string para min\xFA\
  sculas usando o m\xE9todo `.ToLower()` ou `.ToLowerInvariant()`. O primeiro respeita\
  \ as\u2026"
lastmod: '2024-03-13T22:44:46.571450-06:00'
model: gpt-4-1106-preview
summary: "Em C#, voc\xEA pode converter uma string para min\xFAsculas usando o m\xE9\
  todo `.ToLower()` ou `.ToLowerInvariant()`."
title: "Convertendo uma string para min\xFAsculas"
weight: 4
---

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
