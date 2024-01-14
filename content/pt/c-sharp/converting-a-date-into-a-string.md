---
title:                "C#: Convertendo uma data em uma string"
programming_language: "C#"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/c-sharp/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Por que

Ao trabalhar com datas em C#, em algum momento você precisará convertê-las para strings. Isso pode ser necessário para fins de apresentação, armazenamento ou comunicação com outros sistemas. Neste artigo, vamos explorar como realizar essa conversão de maneira eficiente e eficaz.

## Como Fazer

Para converter uma data em uma string no C#, podemos utilizar o método `ToString()` disponível na classe `DateTime`. Veja o exemplo abaixo:

```C#
DateTime data = new DateTime(2021, 08, 31);
string dataString = data.ToString();
Console.WriteLine(dataString);
```

A saída desse código será "31/08/2021 00:00:00", utilizando o formato de data e hora padrão do sistema. Mas e se quisermos um formato específico? Podemos utilizar o método `ToString()` com um parâmetro para indicar o formato desejado, por exemplo:

```C#
DateTime data = new DateTime(2021, 08, 31);
string dataString = data.ToString("dd/MM/yyyy");
Console.WriteLine(dataString);
```

Neste caso, a saída será "31/08/2021", utilizando o formato indicado ("dd/MM/yyyy"). Existem diversos formatos disponíveis, como "MM/dd/yyyy", "yyyy-MM-dd", entre outros. Além disso, é possível formatar também a hora, utilizando códigos específicos como "hh" para horas em formato de 12 horas, "HH" para horas em formato de 24 horas e "mm" para minutos. Você pode encontrar uma lista completa desses códigos na documentação da Microsoft.

## Mergulho Profundo

Ao converter uma data em uma string, é importante levar em consideração a cultura e o fuso horário do usuário. Por padrão, o C# utiliza a cultura e fuso horário do sistema, mas podemos especificar uma cultura e fuso horário diferentes ao utilizar o método `ToString()`, por exemplo:

```C#
DateTime data = new DateTime(2021, 08, 31, 15, 30, 0);
string dataString = data.ToString("G",
    CultureInfo.CreateSpecificCulture("en-US"));
Console.WriteLine(dataString);
```

Neste caso, a saída será "8/31/2021 3:30:00 PM", utilizando o formato e cultura específicos indicados. Além disso, é importante estar ciente de que algumas culturas podem utilizar formatos diferentes para representar uma data, por exemplo, o padrão "MM/dd/yyyy" é comum nos Estados Unidos, enquanto em muitos países europeus é mais comum o formato "dd/MM/yyyy".

## Veja Também

- Documentação da Microsoft sobre `DateTime.ToString()`: https://docs.microsoft.com/pt-br/dotnet/api/system.datetime.tostring
- Lista de códigos de formatação de datas: https://docs.microsoft.com/pt-br/dotnet/standard/base-types/custom-date-and-time-format-strings#custom-date-time-format-strings
- Documentação da Microsoft sobre cultura e fuso horário: https://docs.microsoft.com/pt-br/dotnet/standard/base-types/culture-names
- Exemplo de formatação de datas em diferentes culturas: https://www.dotnetperls.com/datetime-format-locale