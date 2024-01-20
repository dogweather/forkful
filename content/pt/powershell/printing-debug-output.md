---
title:                "Imprimindo saída de debug"
html_title:           "C#: Imprimindo saída de debug"
simple_title:         "Imprimindo saída de debug"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/powershell/printing-debug-output.md"
---

{{< edit_this_page >}}

## O Que & Porquê?

A impressão de debug é uma técnica usada para controlar o fluxo do programa. Os programadores utilizam-na para identificar problemas ou verificar se o código funciona conforme esperado.

## Como Fazer:

Aqui está um exemplo simples de como imprimir debug no PowerShell:

```PowerShell
Write-Verbose "Início do script" -Verbose
$variavel = "PowerShell"
Write-Verbose "A variável está definida como: $variavel" -Verbose
Write-Verbose "Fim do script" -Verbose
```

A saída aparecerá assim:

```Shell
VERBOSE: Início do script
VERBOSE: A variável está definida como: PowerShell
VERBOSE: Fim do script
```

O parâmetro `-Verbose` permite a visualização das mensagens de debug. 

## Mergulho Profundo:

A impressão de debug possui uma longa história em programação que remonta ao primeiro software de computador. No contexto do PowerShell, o cmdlet `Write-Verbose` é geralmente utilizado para debug. Além disso, existe o `Write-Debug`, porém sua saída só é exibida se a preferência `$DebugPreference` estiver definida como 'Continue' ou 'Inquire'.

Uma alternativa ao uso de `Write-Verbose` para debug com a impressão seria a utilização do Interactive PowerShell Debugger. Oferece funcionalidades mais sofisticadas, mas também tem uma maior curva de aprendizagem.

No que diz respeito à implementação, `Write-Verbose` envia objetos para o pipeline de mensagem detalhada de comando, recomendado para mensagens que ajudam a compreender o comportamento de um comando sem modificar seu objecto de saída.

## Veja Também:

Para saber mais sobre o assunto, veja os seguintes recursos:

1. [Debugging Scripts](https://docs.microsoft.com/en-us/powershell/scripting/learn/debugging-scripts?view=powershell-7.1): Veja como utilizar o debugger do PowerShell.
2. [Write-Verbose](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.core/about/about_preference_variables?view=powershell-7.1): Saiba mais sobre o cmdlet Write-Verbose.
3. [PowerShell Tutorial](https://www.janbasktraining.com/blog/powershell-tutorial/): Um tutorial completo em PowerShell para iniciantes.