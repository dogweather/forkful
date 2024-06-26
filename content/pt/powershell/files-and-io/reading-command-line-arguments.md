---
date: 2024-01-20 17:56:36.425983-07:00
description: "Como Fazer: Os argumentos de linha de comando existem desde os primeiros\
  \ dias das interfaces de texto, uma forma de passar informa\xE7\xF5es adicionais\
  \ aos\u2026"
lastmod: '2024-04-05T21:53:47.159473-06:00'
model: gpt-4-1106-preview
summary: "Os argumentos de linha de comando existem desde os primeiros dias das interfaces\
  \ de texto, uma forma de passar informa\xE7\xF5es adicionais aos programas que est\xE3\
  o iniciando."
title: Lendo argumentos da linha de comando
weight: 23
---

## Como Fazer:
```PowerShell
# Exemplo de script.ps1 que lê argumentos de linha de comando
param (
    [String]$nome,
    [Int]$idade
)

Write-Host "Olá, $nome! Você tem $idade anos de idade."

# Exemplo de uso na linha de comando:
# .\script.ps1 -nome "João" -idade 30

# Saída esperada no console:
# Olá, João! Você tem 30 anos de idade.
```

## Mergulho Profundo
Os argumentos de linha de comando existem desde os primeiros dias das interfaces de texto, uma forma de passar informações adicionais aos programas que estão iniciando. No PowerShell, `param` é usado para definir os parâmetros que você quer aceitar. Alternativas incluem o uso direto da variável automática `$args`, que é um array que contém todos os argumentos de linha de comando não associados a um parâmetro nomeado. Quanto à implementação, trabalhar com argumentos de linha de comando é essencial para permitir a integração e automação de scripts com sistemas mais amplos e tarefas de rotina.

## Ver Também
- [Documentação oficial do PowerShell](https://docs.microsoft.com/pt-br/powershell/)
- [Guia de scripting do PowerShell para iniciantes](https://docs.microsoft.com/pt-br/powershell/scripting/learn/ps101/00-introduction?view=powershell-7.1)
