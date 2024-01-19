---
title:                "Encontrando o comprimento de uma string"
html_title:           "C: Encontrando o comprimento de uma string"
simple_title:         "Encontrando o comprimento de uma string"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/powershell/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

# Analisando a Extensão de Strings no PowerShell

## O Que & Por Quê?

Encontrar a extensão de uma string é determinar o número de caracteres que ela possui. Os programadores fazem isso para manipular, validar ou analisar dados textuais com precisão.

## Como Fazer:

Para encontrar o comprimento de uma string no PowerShell, usamos a propriedade '.Length'. Veja o código de exemplo abaixo:

```PowerShell
$string = "Alô, Mundo!"
$string.Length
```

A saída do código acima será:

```
12
```

Ou seja, a string "Alô, Mundo!" tem doze caracteres.

## Mergulho Profundo

A propriedade '.Length' no PowerShell deriva do .NET Framework, no qual o PowerShell é baseado. Existem outras formas de obter o comprimento de uma string em outras linguagens de programação, como o uso de laços for em C ou a função len() em Python.

No PowerShell, entretanto, o uso da propriedade '.Length' é preferível por ser mais eficiente em termos de recursos do sistema e melhor legibilidade do código. Quando a propriedade '.Length' é utilizada, não é necessária uma varredura completa da string, mas apenas a leitura de um valor armazenado internamente.

Lembre-se de que a contagem de caracteres é sensível à cultura e à codificação. Por exemplo, caracteres Unicode, espaços e pontuação são contados como parte da string.

## Ver Também

1. Sobre objects no PowerShell: [link](https://docs.microsoft.com/pt-br/powershell/scripting/learn/deep-dives/everything-about-objects?view=powershell-5.1)
2. Métodos e propriedades de string no .NET: [link](https://docs.microsoft.com/pt-br/dotnet/api/system.string?view=net-5.0)
3. Codificação de caracteres e Unicode: [link](https://docs.microsoft.com/pt-br/windows/uwp/design/globalizing/unicode)