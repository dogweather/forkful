---
title:                "Extraindo substrings"
html_title:           "Bash: Extraindo substrings"
simple_title:         "Extraindo substrings"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/powershell/extracting-substrings.md"
---

{{< edit_this_page >}}

## O Quê e Por Quê?

Extrair substrings é o processo de pegar partes específicas de uma string. Programadores fazem isso para manipular e trabalhar com dados de forma mais eficaz.

## Como Fazer:

Aqui está um exemplo básico de como extrair uma substring de uma string no PowerShell:

```PowerShell
$str = "Olá, mundo do PowerShell!"
$sub = $str.Substring(6, 5)
Write-Output $sub
```

A saída será:

```
mundo
```

Neste exemplo, estamos pedindo uma substring da string `$str`, começando na 6ª posição e pegando 5 caracteres.

## Mergulho Profundo

Historicamente, o PowerShell usa a abordagem baseada em .Net para trabalhar com strings, e o método Substring é parte disso. Existem outras alternativas para extrair substrings no PowerShell, incluindo o uso de regex ou os operadores `-replace` `-split`.

A implementação do método Substring é simples e direta, mas vale a pena notar que a contagem começa em 0, o que significa que o primeiro caractere é encontrado na posição 0.

## Veja Também

Para mais detalhes e alternativas ao método Substring, dê uma olhada em:

- A página oficial da Microsoft sobre string manipulation: https://docs.microsoft.com/pt-br/powershell/scripting/samples/working-with-strings?view=powershell-7.1 
- Um bom guia prático sobre as formas de extrair uma substring no PowerShell: https://www.tutorialspoint.com/powershell/powershell_strings.htm 
- Outros métodos de manipulação de string, como .Remove() e .Insert(): https://devblogs.microsoft.com/scripting/powertip-use-powershell-to-add-insert-new-text-to-string/