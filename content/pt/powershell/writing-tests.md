---
title:                "Escrevendo testes"
date:                  2024-01-19
html_title:           "Arduino: Escrevendo testes"
simple_title:         "Escrevendo testes"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/powershell/writing-tests.md"
---

{{< edit_this_page >}}

## O Que é & Por Que?
Escrever testes é o processo de verificar se o código cumpre o que promete. Programadores fazem isso para garantir qualidade, evitar erros e economizar tempo com manutenção futura.

## Como Fazer:
Vamos usar o Pester, um framework de testes para PowerShell. Instala e roda com estes comandos:
```PowerShell
Install-Module -Name Pester -Force -SkipPublisherCheck
```
Exemplo de um teste simples verificando se 2+2 é igual a 4:
```PowerShell
Describe "Teste de soma simples" {
    It "2 + 2 é igual a 4" {
        $sum = 2 + 2
        $sum | Should -Be 4
    }
}

Invoke-Pester
```
Se passar, mostra:
```
Describing Teste de soma simples
 [+] 2 + 2 é igual a 4 40ms (37ms|3ms)
Tests completed in 40ms
Tests Passed: 1, Failed: 0, Skipped: 0 NotRun: 0
```

## Aprofundamento:
Pester surgiu em 2009, virando o padrão de facto para testes no PowerShell. Alternativas incluem psake e NoSQLT, mas Pester destaca-se pela integração profunda com o PowerShell e suporte da comunidade. O framework permite mock objects e tem suporte para TDD (Test-Driven Development).

## Veja Também:
- [Página oficial do Pester](https://pester.dev)
- [Repositório do Pester no GitHub](https://github.com/pester/Pester)
