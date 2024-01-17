---
title:                "Convertendo uma data em uma string"
html_title:           "C#: Convertendo uma data em uma string"
simple_title:         "Convertendo uma data em uma string"
programming_language: "C#"
category:             "C#"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/c-sharp/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

O Que & Por Que?

Convertendo uma data em uma string é um processo em que um programador transforma uma data, que é um formato específico para armazenar informações de tempo, em uma string, que é uma cadeia de caracteres de texto, para que ela possa ser lida e entendida pelos usuários do programa. Isso é importante porque permite que os usuários interajam com as informações de data de maneira mais amigável e legível.

Como Fazer:

Para converter uma data em uma string em C#, utilize o método `ToString()` em uma instância de `DateTime`. Esse método aceita um argumento opcional para especificar o formato da string de saída. Veja um exemplo abaixo:

```C#
DateTime data = new DateTime(2021, 05, 26);
string dataString = data.ToString("yyyy-MM-dd");
```

A saída desse código seria "2021-05-26", pois especificamos que queremos o ano, seguido de um traço, seguido do mês, seguido de outro traço, seguido do dia. Existem diversos formatos disponíveis, como "dd/MM/yyyy" para exibir a data no formato brasileiro.

Deep Dive:

Historicamente, a conversão de data em string era feita de maneira mais complicada e manual, com o programador tendo que formatar a data de acordo com o seu objetivo. Com o avanço das linguagens de programação, foi possível padronizar esse processo criando métodos específicos e formatos de data padrão.

Além do método `ToString()`, em C# também é possível utilizar a classe `DateTimeFormat` para obter formatos de data padrão ou criar um formato personalizado. Além disso, existem outras maneiras de representar uma data no formato de string, como utilizando o tipo `StringBuilder` ou concatenando strings.

Ver Também:

Para mais informações e exemplos de conversão de data em string em C#, veja a documentação oficial da Microsoft: https://docs.microsoft.com/pt-br/dotnet/standard/base-types/custom-date-and-time-format-strings.

Também é possível encontrar diversas bibliotecas e ferramentas que facilitam essa conversão, como o `DateExtensions` para formatação mais flexível ou o `Humanizer` para deixar a data mais amigável para os usuários.