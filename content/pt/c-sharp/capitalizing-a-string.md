---
title:                "Capitalização de uma string"
html_title:           "C#: Capitalização de uma string"
simple_title:         "Capitalização de uma string"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/c-sharp/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## O que e porque?

Capitalizar uma string significa mudar a primeira letra de cada palavra para maiúscula. Os programadores costumam fazer isso para melhorar a legibilidade e apresentação do texto ao usuário final.

## Como fazer:

O código a seguir mostra como capitalizar uma string em C# usando o método `ToTitleCase` da classe `TextInfo`. O resultado será exibido no console.

```C#
using System.Globalization;

string texto = "programação é legal";
TextInfo textInfo = new CultureInfo("pt-BR", false).TextInfo;
string textoCapitalizado = textInfo.ToTitleCase(texto);
Console.WriteLine(textoCapitalizado);
```

```
Programação É Legal
```

## Mergulho Profundo:

No passado, em algumas línguas, apenas a primeira letra de uma frase era maiúscula. Mais tarde, o uso de maiúsculas e minúsculas foi adotado para melhorar a leitura e compreensão. Mas em alguns sistemas, apenas letras maiúsculas eram permitidas, e capitalizar uma string permitia que esses sistemas aceitassem o texto.

Uma alternativa à capitalização de uma string é apenas forçar todas as letras para minúsculas ou maiúsculas, mas isso pode resultar em palavras ou nomes incorretos. Além disso, ao usar o método `ToTitleCase`, ele leva em consideração as regras de capitalização para cada idioma.

Na implementação do método `ToTitleCase`, a classe `TextInfo` utiliza as convenções do idioma do sistema operacional para fazer a capitalização. Isso significa que o resultado pode ser diferente dependendo do idioma do usuário.

## Veja também:

- Documentação oficial da Microsoft sobre o método `ToTitleCase`: https://docs.microsoft.com/pt-br/dotnet/api/system.globalization.textinfo.totitlecase?view=net-5.0
- Explicação da diferença entre `ToLower` e `ToTitleCase`: https://stackoverflow.com/a/186464
- Lista de códigos de idiomas usados pelo `ToTitleCase`: https://msdn.microsoft.com/library/System.Globalization.CultureInfo(v=vs.110).aspx