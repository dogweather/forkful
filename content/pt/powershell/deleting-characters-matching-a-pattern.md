---
title:                "Excluindo caracteres que correspondem a um padrão"
html_title:           "Arduino: Excluindo caracteres que correspondem a um padrão"
simple_title:         "Excluindo caracteres que correspondem a um padrão"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/powershell/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## O que & Porquê?

Exclusão de caracteres combinando com um padrão é um método em que certos caracteres especificados são removidos de uma string. Programadores fazem isso para manipular dados de texto, limpar entrada de usuários e processar arquivos.

## Como fazer:
No PowerShell, usamos o método -replace para deletar caracteres que combinam com um padrão. Aqui está como você faria:

```PowerShell
# Defina a string
$string = "Bom dia, mundo!"

# Defina o padrão para excluir todas as vírgulas
$pattern = ","

# Use -replace para excluir vírgulas
$newString = $string -replace $pattern, ""
```

A saída será:

```PowerShell
"Bom dia mundo!"
```

## Mergulho Profundo:
A prática de excluir caracteres que combinam com um padrão está presente desde os primórdios da programação, usada para tratar e limpar dados. No PowerShell, -replace não é a única opção. Funções como -split ou -join também podem ser usadas, dependendo do contexto.

O método -replace usa regex (expressões regulares) para combinar o padrão. Se você não especificar um segundo parâmetro, ele excluirá todos os caracteres que correspondem ao padrão. Razão pela qual neste exemplo, a vírgula é removida.

## Veja também:
Para aprender mais sobre tratamento de strings e expressões regulares no PowerShell, confira estes links:
1. Documentação do Microsoft sobre o método -replace em PowerShell: https://docs.microsoft.com/pt-br/powershell/module/microsoft.powershell.core/about/about_comparison_operators?view=powershell-7.1#replacement-operator
2. Tutorial da Regex para iniciantes: https://www.regular-expressions.info/tutorial.html
3. Documentação do Microsoft sobre tratamento de strings em PowerShell: https://docs.microsoft.com/pt-br/powershell/scripting/learn/deep-dives/everything-about-string-substitutions?view=powershell-7.1