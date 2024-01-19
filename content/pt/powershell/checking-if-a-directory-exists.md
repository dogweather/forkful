---
title:                "Verificando se um diretório existe"
html_title:           "C#: Verificando se um diretório existe"
simple_title:         "Verificando se um diretório existe"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/powershell/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## O Que & Por Quê?

Verificar se um diretório existe é a prática de fazer uma consulta ao seu sistema operacional a partir de um script para ver se um determinado diretório (uma "pasta") já existe ou não. Fazemos isso para evitar erros quando tentamos fazer operações em diretórios que podem não estar lá.

## Como Fazer:

Para verificar se um diretório existe em PowerShell, você pode usar o comando `Test-Path`. Aqui está um exemplo simplificado:

```PowerShell
$directoryPath = "C:\AlgumCaminho\AlgumDiretorio"

if (Test-Path $directoryPath) 
{
    Write-Output "O diretório existe."
} 
else 
{
    Write-Output "O diretório não existe."
}
```

Se o diretório "C:\AlgumCaminho\AlgumDiretorio" existir, este script imprimirá "O diretório existe.". Caso contrário, ele dirá "O diretório não existe.".

## Aprofundando

O `Test-Path` foi introduzido no PowerShell 1.0, como uma forma útil de verificar a existência de diretórios e arquivos. Alternativamente, em versões mais antigas do DOS, você poderia usar `IF EXIST`, mas essa opção não é tão confiável ou versátil quanto o `Test-Path` em PowerShell.

A verificação de existência de diretórios é muitas vezes um passo vital antes de tentar criar um novo diretório, copiar arquivos para um diretório específico ou fazer qualquer outra operação que exija que um determinado diretório esteja presente.

De fato, PowerShell, com sua riqueza de comandos e operações, permite várias maneiras de realizar essa tarefa. Além de `Test-Path`, você também pode usar `Get-Item` e `Get-ChildItem`, combinado com alguma lógica de manipulação de erro, para verificar se um diretório existe.

## Veja Também

1. Documentação oficial do PowerShell em `Test-Path`: (https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.management/test-path)
2. Uma discussão detalhada no StackOverflow sobre "Verificar se a pasta existe em PowerShell": (https://stackoverflow.com/questions/2346005/test-if-a-directory-exists-in-a-shell-script)
3. Aprenda a criar e modificar diretórios em PowerShell: (https://www.tutorialspoint.com/powershell/powershell_working_with_directories.htm)