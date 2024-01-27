---
title:                "Capitalizando uma string"
date:                  2024-01-19
html_title:           "Bash: Capitalizando uma string"
simple_title:         "Capitalizando uma string"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/powershell/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## O Que & Porquê?
Capitalizar uma string significa tornar a primeira letra de cada palavra em maiúscula, mantendo as outras em minúscula. Programadores fazem isso para padronizar dados, como nomes próprios, ou para atender a requisitos estilísticos em interfaces de usuário, documentos e relatórios.

## Como fazer:
Com o PowerShell, capitalizar uma string é simples. Aqui estão algumas maneiras:

```PowerShell
# Usando o método ToTitleCase da classe CultureInfo
$texto = "isto é um exemplo"
$culture = [System.Globalization.CultureInfo]::CurrentCulture
$titleCaseText = $culture.TextInfo.ToTitleCase($texto)
Write-Output $titleCaseText
```

Saída da amostra:
```
Isto É Um Exemplo
```

## Aprofundamento:
Capitalizar palavras numa string não é uma invenção recente. Isso remonta ao desenvolvimento das primeiras linguagens de programação que lidam com texto, e regras tipográficas muito antes disso.

Alternativas:
- Além do método `ToTitleCase`, você pode usar expressões regulares ou iterar pela string e capitalizar cada palavra manualmente, mas isso geralmente é desnecessário e pode ser mais propenso a erros.
  
Detalhes de Implementação:
- O método `ToTitleCase` não capitaliza palavras já em maiúsculas por questões de design, como em siglas. Se você precisar de um comportamento diferente, terá de implementar uma solução personalizada.

## Veja Também:
- Documentação da classe `TextInfo`: [Microsoft Docs](https://docs.microsoft.com/en-us/dotnet/api/system.globalization.textinfo?view=net-6.0)
