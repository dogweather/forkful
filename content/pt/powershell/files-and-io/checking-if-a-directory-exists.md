---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:08:12.700449-07:00
description: "No PowerShell, verificar se um diret\xF3rio existe \xE9 uma tarefa comum\
  \ que ajuda os scripts a tomarem decis\xF5es baseadas na estrutura do sistema de\u2026"
lastmod: 2024-02-19 22:05:05.868621
model: gpt-4-0125-preview
summary: "No PowerShell, verificar se um diret\xF3rio existe \xE9 uma tarefa comum\
  \ que ajuda os scripts a tomarem decis\xF5es baseadas na estrutura do sistema de\u2026"
title: "Verificando se um diret\xF3rio existe"
---

{{< edit_this_page >}}

## O quê & Por quê?
No PowerShell, verificar se um diretório existe é uma tarefa comum que ajuda os scripts a tomarem decisões baseadas na estrutura do sistema de arquivos—como evitar erros ao confirmar que um diretório-alvo está no lugar antes de tentar ler ou escrever nele. É essencial para garantir que seu script se comporte de maneira confiável em ambientes diversos.

## Como fazer:
O PowerShell oferece uma maneira direta de verificar a presença de um diretório usando o cmdlet `Test-Path`. Esse cmdlet retorna um valor Booleano indicando se o caminho especificado existe. Veja como você pode usá-lo:

```powershell
# Verificar se um diretório existe
$directoryPath = "C:\CaminhoExemplo"
$directoryExists = Test-Path -Path $directoryPath
Write-Output "O diretório existe? $directoryExists"
```

Saída de amostra para um diretório que existe:

```
O diretório existe? Verdadeiro
```

E para um diretório que não existe:

```
O diretório existe? Falso
```

Para scripts mais complexos, especialmente aqueles que interagem com compartilhamentos de rede ou armazenamento em nuvem, você pode precisar de verificações adicionais ou funcionalidades não disponíveis diretamente através do `Test-Path`. Nesses casos, a utilização de módulos ou bibliotecas do PowerShell de terceiros pode ser benéfica, embora a maioria das tarefas de rotina possa ser realizada com os cmdlets internos do PowerShell. Até minha última atualização de conhecimento, não houve uma biblioteca de terceiros amplamente adotada especificamente para verificar a existência de diretórios além do que o `Test-Path` oferece, principalmente porque o `Test-Path` em si é robusto e eficiente para esse propósito.
