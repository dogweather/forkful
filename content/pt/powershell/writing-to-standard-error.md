---
title:                "Escrevendo para o erro padrão"
html_title:           "PowerShell: Escrevendo para o erro padrão"
simple_title:         "Escrevendo para o erro padrão"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/powershell/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## O Que e Por Que?

Escrever para o erro padrão é uma técnica usada pelos programadores para exibir mensagens de erro ou avisos durante a execução de um código PowerShell. Essas mensagens são úteis para identificar e corrigir possíveis problemas no código, tornando o processo de depuração mais fácil e eficiente.

## Como Fazer:

Para escrever para o erro padrão em um script PowerShell, você pode usar o cmdlet "Write-Error". Veja um exemplo:

```PowerShell
Write-Error "Ocorreu um erro ao processar o arquivo."
```

Isso irá exibir a mensagem especificada no erro padrão, juntamente com informações sobre a linha e o arquivo onde ocorreu o erro. O resultado seria algo como:

```
Write-Error : Ocorreu um erro ao processar o arquivo.
At line:1 char:1
+ Write-Error "Ocorreu um erro ao processar o arquivo."
+ ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    + CategoryInfo          : NotSpecified: (:) [Write-Error], WriteErrorException
    + FullyQualifiedErrorId : Microsoft.PowerShell.Commands.WriteErrorException

```

Você também pode personalizar a mensagem de erro com cores e ícones para torná-la mais visível e fácil de identificar. Veja outro exemplo:

```PowerShell
Write-Error "Out of memory" -ForegroundColor Red -ErrorAction Stop
```

Isso resultaria em:

![Exemplo de mensagem de erro personalizada](https://i.imgur.com/kV6FLIg.png)

## Mergulho Profundo:

Escrever para o erro padrão é uma prática comum em várias linguagens de programação, incluindo PowerShell. A ideia principal é fornecer ao programador uma maneira de identificar e corrigir erros de forma rápida e eficaz durante o desenvolvimento de um código.

Como alternativa ao cmdlet "Write-Error", você também pode usar o operador "&" para escrever para o erro padrão. Isso pode ser útil quando você precisa lidar com vários comandos em um único bloco de código. Veja um exemplo:

```PowerShell
& {
    Get-ChildItem -Path "C:\Arquivos" -ErrorVariable erro
    Write-Error $erro
}
```

Essa técnica permite que você execute um comando e, em seguida, escreva o erro SOBRE ele, em vez de substituí-lo por uma mensagem de erro personalizada.

O cmdlet "Write-Host" é outra opção, mas geralmente é usado para exibir mensagens de status e informações gerais, não para relatar erros durante a execução do código.

## Veja Também:

Saiba mais sobre como escrever mensagens de erro em código PowerShell:
