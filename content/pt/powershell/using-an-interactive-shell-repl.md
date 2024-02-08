---
title:                "Usando um shell interativo (REPL)"
aliases:
- pt/powershell/using-an-interactive-shell-repl.md
date:                  2024-01-26T04:16:54.292771-07:00
model:                 gpt-4-0125-preview
simple_title:         "Usando um shell interativo (REPL)"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/powershell/using-an-interactive-shell-repl.md"
---

{{< edit_this_page >}}

## O Que & Por Que?
O shell interativo, ou Loop de Ler-Avaliar-Imprimir (REPL), permite que você digite comandos PowerShell e obtenha feedback imediato. Programadores o usam para testar rapidamente trechos de código, depurar ou aprender novos comandos sem escrever um script completo.

## Como:
Inicie o PowerShell e você estará no REPL. Experimente o Cmdlet `Get-Date`:

```PowerShell
PS > Get-Date
```

Você deve ver a data e a hora atual como saída:

```PowerShell
Quarta-feira, 31 de março de 2023 12:34:56
```

Agora, encadeie comandos. Vamos ordenar processos pelo uso de memória:

```PowerShell
PS > Get-Process | Sort-Object WS -Descending | Select-Object -First 5
```

Isso exibe os 5 principais processos por tamanho do conjunto de trabalho (uso de memória).

## Aprofundamento
O REPL do PowerShell tem suas raízes no shell Unix e em outros shells de linguagens dinâmicas como o Python. É um ambiente de execução de comandos interativo de um único usuário. Diferente de uma linguagem compilada onde você escreve aplicações inteiras e depois compila, um ambiente REPL permite que você escreva e execute código uma linha por vez. O PowerShell também suporta a execução de scripts para tarefas maiores.

Alternativas para o Windows incluem o Prompt de Comando ou outros REPLs específicos de linguagem como o IPython. No mundo Unix/Linux, shells como bash ou zsh servem a uma função semelhante.

A implementação do PowerShell utiliza uma aplicação host para executar o shell. Embora o PowerShell.exe no Windows seja o mais comum, outros como o Integrated Scripting Environment (ISE) ou o terminal integrado do Visual Studio Code também podem servir como host.

## Veja Também
- [Sobre o PowerShell](https://docs.microsoft.com/en-us/powershell/scripting/overview)
- [StackOverflow: PowerShell](https://stackoverflow.com/questions/tagged/powershell)
