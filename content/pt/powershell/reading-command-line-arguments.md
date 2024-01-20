---
title:                "Lendo argumentos de linha de comando"
html_title:           "Arduino: Lendo argumentos de linha de comando"
simple_title:         "Lendo argumentos de linha de comando"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/powershell/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

# Lidando com Argumentos da Linha de Comando em PowerShell

## O Que & Por Quê?

Os argumentos da linha de comando são parâmetros adicionais que você passa para um script PowerShell na linha de comando. Eles são úteis para personalizar o comportamento de um script sem ter que modificá-lo.

## Como:

Para capturar argumentos da linha de comando em PowerShell, você pode usar a variável automática `$args`, que é um array de argumentos não reconhecidos pelo PowerShell. Aqui está um exemplo simples:

```PowerShell
#Test-Script.ps1
param (
  $PrimeiroNome,
  $Sobrenome
)

Write-Output "Olá $PrimeiroNome $Sobrenome!"
```

Você pode invocar este script usando:

```PowerShell
.\Test-Script.ps1 -PrimeiroNome "João" -Sobrenome "Silva"
```

O script irá imprimir:

```PowerShell
Olá João Silva!
```

## Mergulho Profundo

O uso de argumentos na linha de comando remonta à era dos sistemas operacionais baseados em texto, quando o usuário interagia com o computador principalmente por meio de comandos de texto.

Os argumentos podem ser usados para passar informações tanto para o script PowerShell quanto para os comandos dentro do script. Eles também permitem scripts dinâmicos, tornando-os mais reutilizáveis.

A alternativa ao uso de `$args` seria definir variáveis diretamente dentro do script, mas isso tira a flexibilidade de alterar o comportamento do script sem ter que editá-lo.

Tenha isso em mente ao usar `$args`: ele ignora argumentos que são reconhecidos, ou seja, se sua função ou script tem um parâmetro chamado 'PrimeiroNome', e você passa esse parâmetro na linha de comando, ele não aparecerá no `$args`.

## Veja Também

* Documentação oficial do Microsoft PowerShell: [Sobre Argumentos da Linha de Comando](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.core/about/about_command_line_arguments?view=powershell-7.1)
* Guia de Scripting: [Usando Argumentos da Linha de Comando](https://www.computerperformance.co.uk/powershell/powershell_args_argumentlist/)
* Tutorial de vídeo: [PowerShell e Argumentos da Linha de Comando](https://www.youtube.com/watch?v=8uAo8X00Q9o)