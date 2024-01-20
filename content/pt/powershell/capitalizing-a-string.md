---
title:                "Capitalizando uma string"
html_title:           "PowerShell: Capitalizando uma string"
simple_title:         "Capitalizando uma string"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/powershell/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## O Quê e Porquê?

Capitalizar uma string é o processo de transformar a primeira letra de cada palavra em uma letra maiúscula. Os programadores fazem isso para melhorar a legibilidade ou para a formatação de dados de texto na apresentação ao usuário.

## Como fazer:

No PowerShell, pode capitalizar uma string usando o método `.ToTitleCase()`. Aqui está um exemplo:

```PowerShell
$str = "olá, mundo!"
$cultura = new-object System.Globalization.CultureInfo("pt-BR")
$textInfo = $cultura.TextInfo
$resultado = $textInfo.ToTitleCase($str)

Escreva-Saída $resultado
```

Isto te dará a seguinte saída:

```PowerShell
Olá, Mundo!
```

## Aprofundamento:

As strings são um elemento fundamental em muitos programas, e manipular corretamente essas strings é crítico para muitos sistemas. A função ToTitleCase() foi introduzida para facilitar este processo, fornecendo uma maneira rápida e eficiente para capitalizar strings. 

Como alternativa, pode usar outros métodos para chegar no mesmo resultado. Por exemplo, o método `.ToUpper()` capitalizaria toda a string, não apenas as primeiras letras de cada palavra. 

Tecnicamente, o método `.ToTitleCase()` funciona analisando cada caractere da string. Se o caractere anterior era um tipo de caractere não-letral (como um espaço, símbolo ou número), ele converte o próximo caractere em letra maiúscula.

## Veja Também:

- [Documentação Oficial do PowerShell](https://docs.microsoft.com/pt-br/powershell/)
- [Como Usar o Método ToTitleCase](https://docs.microsoft.com/pt-br/dotnet/api/system.globalization.textinfo.totitlecase)
- [Manipulação de Strings no PowerShell](https://devblogs.microsoft.com/scripting/manipulating-strings-with-powershell/)