---
date: 2024-01-26 03:36:54.379469-07:00
description: "Refatora\xE7\xE3o \xE9 o processo de reestruturar o c\xF3digo de computador\
  \ existente sem alterar seu comportamento externo, visando melhorar atributos n\xE3\
  o funcionais\u2026"
lastmod: '2024-03-13T22:44:46.805214-06:00'
model: gpt-4-0125-preview
summary: "Refatora\xE7\xE3o \xE9 o processo de reestruturar o c\xF3digo de computador\
  \ existente sem alterar seu comportamento externo, visando melhorar atributos n\xE3\
  o funcionais do software."
title: "Refatora\xE7\xE3o"
weight: 19
---

## O Que & Por Quê?
Refatoração é o processo de reestruturar o código de computador existente sem alterar seu comportamento externo, visando melhorar atributos não funcionais do software. Programadores refatoram código para torná-lo mais limpo, mais eficiente e mais fácil de entender, o que facilita a manutenção e aprimoramentos futuros.

## Como:
O PowerShell não tem uma ferramenta dedicada à refatoração incorporada, mas você ainda pode limpar seu código para melhorar a legibilidade e o desempenho. Considere uma função que está fazendo demais e como poderíamos refatorá-la para clareza:

```PowerShell
function Get-InventoryData {
    # Função original combinando recuperação e formatação de dados
    $data = Get-Content -Path 'C:\inventory-list.txt'
    $inventoryData =  $data | ForEach-Object {
        $fields = $_ -split ','
        [PSCustomObject]@{
            ItemID = $fields[0]
            Name   = $fields[1]
            Count  = $fields[2]
            Price  = $fields[3]
        }
    }
    $inventoryData | Format-Table -AutoSize
}

# Refatorado em funções separadas
function Import-InventoryData {
    param($Path)
    Get-Content -Path $Path | ForEach-Object {
        $fields = $_ -split ','
        [PSCustomObject]@{
            ItemID = $fields[0]
            Name   = $fields[1]
            Count  = $fields[2]
            Price  = $fields[3]
        }
    }
}

function Format-InventoryData {
    param($Data)
    $Data | Format-Table -AutoSize
}

# Uso
$inventory = Import-InventoryData -Path 'C:\inventory-list.txt'
Format-InventoryData -Data $inventory
```

Saída de exemplo:

```
ItemID Name            Count Price
------ ----            ----- -----
1001   Widget Tipo A   50    9.99
1002   Gadget Tipo B   20    14.99
```

## Aprofundamento
A refatoração em programação tem suas raízes nos primórdios do desenvolvimento de software, embora tenha sido formalizada como prática na década de 1990. O livro de Martin Fowler "Refatoração: Melhorando o design do código existente" é uma das obras fundamentais sobre o assunto, enfatizando a importância da refatoração para alcançar um código limpo.

Embora o PowerShell não venha com ferramentas específicas de refatoração como algumas Ambientes de Desenvolvimento Integrado (IDEs) para outras linguagens (como Eclipse ou Visual Studio), ainda é possível praticar bons princípios de refatoração manualmente. O ponto chave a lembrar é que a refatoração não é apenas sobre mudar código por mudar, mas fazer modificações intencionais e preservando o comportamento que aprimoram a estrutura e o design do código.

Alternativas à refatoração manual no PowerShell incluem o uso de IDEs que suportam a linguagem, como o Visual Studio Code com a extensão do PowerShell, que oferece recursos como formatação de código e capacidades básicas de refatoração. Para refatorações mais significativas, você pode considerar o uso de testes Pester para garantir que as mudanças não alterem a funcionalidade.

Além disso, a implementação da refatoração pode envolver mudanças mais sistêmicas como a modularização, onde o código é dividido em módulos ou funções reutilizáveis, melhorando a aderência ao princípio DRY (Don't Repeat Yourself). Outras técnicas comuns de refatoração incluem renomear para clareza, remover código duplicado e reduzir a complexidade da lógica condicional.

## Veja Também
Para se aprofundar, aqui estão alguns recursos:

- Livro de Refatoração de Martin Fowler: [_Refatoração: Melhorando o Design do Código Existente_](https://martinfowler.com/books/refactoring.html)
- Testando código refatorado com Pester: [Framework de Teste Pester](https://pester.dev/)
- Melhores Práticas do PowerShell: [Guia de Melhores Práticas e Estilo do PowerShell](https://poshcode.gitbooks.io/powershell-practice-and-style/)
