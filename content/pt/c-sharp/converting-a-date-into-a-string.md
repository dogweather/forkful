---
title:                "C#: Convertendo uma data em uma string"
simple_title:         "Convertendo uma data em uma string"
programming_language: "C#"
category:             "C#"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/c-sharp/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Por que converter uma data em uma string
Conversão de uma data em uma string pode ser útil ao trabalhar com diferentes formatos de data ou quando é necessário exibir a data em um formato específico. Além disso, a conversão permite que a data seja manipulada e armazenada como uma string em bancos de dados e outros sistemas.

## Como fazer
```C#
// Exemplo de código para converter uma data em uma string no formato "dd/MM/yyyy":
DateTime data = DateTime.Today;
string dataString = data.ToString("dd/MM/yyyy"); 

Console.WriteLine(dataString); // Output: 11/06/2021
```

```C#
// Exemplo de código para converter uma data em uma string no formato "MMMM, yyyy":
DateTime data = new DateTime(2021, 05, 10);
string dataString = data.ToString("MMMM, yyyy");

Console.WriteLine(dataString); // Output: maio, 2021
```

## Aprofundando
Ao converter uma data em uma string, é importante considerar os diferentes formatos de data e como eles afetam o resultado final. Além disso, é possível especificar o idioma e a cultura para garantir que a data seja exibida corretamente. Outro detalhe importante é o uso de máscaras de formatação para controlar como a data é apresentada.

## Veja também
- [Documentação oficial da Microsoft sobre formatação de datas em C#](https://docs.microsoft.com/pt-br/dotnet/standard/base-types/custom-date-and-time-format-strings)
- [Tutorial sobre conversão de data em string em C#](https://www.devmedia.com.br/convertendo-data-para-string-e-string-para-data-em-csharp/27533)
- [Vídeo explicando como converter uma data em uma string com C#](https://www.youtube.com/watch?v=TPo7chvAg44)