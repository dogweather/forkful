---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:31:36.328771-07:00
description: "Escrever testes em PowerShell envolve criar scripts que validam automaticamente\
  \ a funcionalidade do seu c\xF3digo PowerShell, garantindo que ele se comporte\u2026"
lastmod: '2024-03-13T22:44:46.800500-06:00'
model: gpt-4-0125-preview
summary: "Escrever testes em PowerShell envolve criar scripts que validam automaticamente\
  \ a funcionalidade do seu c\xF3digo PowerShell, garantindo que ele se comporte\u2026"
title: Escrevendo testes
weight: 36
---

## O Que & Porquê?

Escrever testes em PowerShell envolve criar scripts que validam automaticamente a funcionalidade do seu código PowerShell, garantindo que ele se comporte conforme esperado. Programadores fazem isso para pegar bugs mais cedo, simplificar a manutenção do código e garantir que modificações no código não quebrem inadvertidamente a funcionalidade existente.

## Como fazer:

O PowerShell não possui um framework de teste integrado, mas o Pester, um módulo de terceiros popular, é amplamente utilizado para escrever e executar testes. Aqui está como começar com o Pester para testar suas funções PowerShell.

Primeiro, instale o Pester se ainda não o fez:

```powershell
Install-Module -Name Pester -Scope CurrentUser -Force
```

Em seguida, considere que você tem uma função PowerShell simples que deseja testar, salva como `MyFunction.ps1`:

```powershell
function Get-MultipliedNumber {
    param (
        [int]$Number,
        [int]$Multiplier = 2
    )

    return $Number * $Multiplier
}
```

Para testar esta função com o Pester, crie um script de teste denominado `MyFunction.Tests.ps1`. Neste script, utilize os blocos `Describe` e `It` do Pester para definir os casos de teste:

```powershell
# Importar a função a ser testada
. .\MyFunction.ps1

Describe "Testes Get-MultipliedNumber" {
    It "Multiplica o número por 2 quando nenhum multiplicador é fornecido" {
        $result = Get-MultipliedNumber -Number 3
        $result | Should -Be 6
    }

    It "Multiplica corretamente o número pelo multiplicador dado" {
        $result = Get-MultipliedNumber -Number 3 -Multiplier 3
        $result | Should -Be 9
    }
}
```

Para executar os testes, abra o PowerShell, navegue até o diretório que contém o seu script de teste e use o comando `Invoke-Pester`:

```powershell
Invoke-Pester .\MyFunction.Tests.ps1
```

A saída de exemplo será assim, indicando se seus testes passaram ou falharam:

```
Starting discovery in 1 files.
Discovery finished in 152ms.
[+] C:\caminho\para\MyFunction.Tests.ps1 204ms (182ms|16ms)
Tests completed in 204ms
Tests Passed: 2, Failed: 0, Skipped: 0 NotRun: 0
```

Esta saída mostra que ambos os testes passaram, dando-lhe confiança de que sua função `Get-MultipliedNumber` se comporta conforme esperado nos cenários que você testou.
