---
title:                "Lendo argumentos da linha de comando"
html_title:           "PowerShell: Lendo argumentos da linha de comando"
simple_title:         "Lendo argumentos da linha de comando"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/powershell/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## O que e Por Que?

Ler argumentos da linha de comando é quando um programa pode aceitar inputs para executar diferentes tarefas, como alterar seu comportamento ou fornecer informações ao usuário. Os programadores fazem isso para tornar seus programas mais flexíveis e interativos.

## Como Fazer:

```PowerShell
#Vamos supor que seu programa requer um argumento para rodar:
[CmdletBinding()]
Param(
    [Parameter(Mandatory=$true)]
    [string]$Nome
)

Write-Host "Olá $Nome, bem-vindo ao meu programa!"
```

**Exemplo de input:**
```PowerShell
.\meu_programa.ps1 -Nome "João"
```

**Output:**
```
Olá João, bem-vindo ao meu programa!
```

## Mergulho Profundo:

*Contexto histórico:* A ideia de argumentos da linha de comando vem dos primórdios da computação, quando os programas precisavam ser executados via linha de comando antes do surgimento de interfaces gráficas.

*Alternativas:* Além de ler argumentos da linha de comando, os programadores também podem usar outros métodos para receber inputs, como ler de arquivos ou interagir com o usuário através de prompts.

*Detalhes de implementação:* Em PowerShell, podemos ler argumentos usando o cmdlet "Param", que permite definir parâmetros obrigatórios ou opcionais e seus respectivos tipos de dados.

## Veja Também:

- [Documentação PowerShell Param](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.core/about/about_functions_advanced_parameters?view=powershell-7)
- [LinkedIn Learning - PowerShell: Working with Command-Line Arguments](https://www.linkedin.com/learning/powershell-working-with-command-line-arguments?trk=insiders_678-cliascc_PowerShell%3A%20Working%20with%20Command-Line%20Arguments_i4_learning)