---
title:                "Escrevendo no erro padrão"
html_title:           "Arduino: Escrevendo no erro padrão"
simple_title:         "Escrevendo no erro padrão"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/powershell/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## O Que é & Porquê?
Escrever no 'standard error' (stderr) é direcionar mensagens de erro para um stream específico, separando-as da saída padrão (stdout). Programadores fazem isso para diagnosticar problemas e permitir que usuários ou outros programas tratem erros de maneira apropriada.

## Como Fazer:
```PowerShell
# Escrevendo no stderr
Write-Error "Este é um erro!"

# Redirecionando stderr para um arquivo
Write-Error "Este é um erro!" 2> errolog.txt

# Capturando stderr em uma variável
$erro = Write-Error "Este é um erro!" 2>&1
```

Saída esperada na tela para o primeiro exemplo:
```
Write-Error : Este é um erro!
At line:1 char:1
+ Write-Error "Este é um erro!"
+ ~~~~~~~~~~~~~~~~~~~~~~~~~~~
    + CategoryInfo          : NotSpecified: (:) [Write-Error], WriteErrorException
    + FullyQualifiedErrorId : Microsoft.PowerShell.Commands.WriteErrorException
```

No segundo exemplo, nenhum output é exibido na tela; a mensagem de erro é salva no arquivo `errolog.txt`.

No terceiro exemplo, o erro é capturado na variável `$erro` e mais nada é exibido na tela.

## Aprofundando
Historicamente, dividir a saída em streams dedicados (stdout e stderr) deriva de práticas de sistemas Unix para facilitar o processamento e filtragem de dados. Em PowerShell, `Write-Error` é um cmdlet específico para enviar mensagens de erro ao stderr, porém existem alternativas como `Throw` para exceções e `$host.ui.WriteErrorLine()` para um controle mais fino. Escrever para stderr é essencial em scripts que exigem uma separação clara entre conteúdo de saída (dados) e mensagens de erro para fins de log e análise.

## Veja Também
- Documentação do PowerShell sobre `Write-Error`: https://docs.microsoft.com/pt-br/powershell/module/microsoft.powershell.utility/write-error
- Guia sobre redirecionamento no PowerShell, incluindo stderr: https://docs.microsoft.com/pt-br/powershell/scripting/learn/deep-dives/everything-about-redirects
- Informações sobre handling de erros no PowerShell: https://docs.microsoft.com/pt-br/powershell/scripting/learn/deep-dives/everything-about-exceptions-errors
