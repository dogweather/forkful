---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:20:42.682287-07:00
description: "Trabalhar com arquivos CSV (Comma-Separated Values ou Valores Separados\
  \ por V\xEDrgula) \xE9 uma tarefa comum para o gerenciamento e manipula\xE7\xE3\
  o de dados de uma\u2026"
lastmod: '2024-03-13T22:44:46.819237-06:00'
model: gpt-4-0125-preview
summary: "Trabalhar com arquivos CSV (Comma-Separated Values ou Valores Separados\
  \ por V\xEDrgula) \xE9 uma tarefa comum para o gerenciamento e manipula\xE7\xE3\
  o de dados de uma\u2026"
title: Trabalhando com CSV
weight: 37
---

## O Que & Por Quê?

Trabalhar com arquivos CSV (Comma-Separated Values ou Valores Separados por Vírgula) é uma tarefa comum para o gerenciamento e manipulação de dados de uma forma estruturada e tabular. Programadores frequentemente realizam essa operação para importar, exportar ou manipular dados de maneira eficiente para várias aplicações, como análise de dados, relatórios ou até mesmo para alimentar aplicações web.

## Como fazer:

### Lendo um Arquivo CSV

Para ler de um arquivo CSV, use o cmdlet `Import-Csv`. Este cmdlet lê o arquivo e o converte em objetos personalizados do PowerShell para cada linha.

```powershell
# Importando um arquivo CSV
$data = Import-Csv -Path "C:\Data\users.csv"
# Exibindo o conteúdo
$data
```

**Saída de Exemplo:**

```
Name    Age    City
----    ---    ----
John    23     New York
Doe     29     Los Angeles
```

### Escrevendo em um Arquivo CSV

De forma inversa, para escrever dados em um arquivo CSV, o cmdlet `Export-Csv` é usado. Este cmdlet pega objetos de entrada e os converte em um formato CSV.

```powershell
# Criando um objeto para exportar
$users = @(
    [PSCustomObject]@{Name='John'; Age='23'; City='New York'},
    [PSCustomObject]@{Name='Doe'; Age='29'; City='Los Angeles'}
)

# Exportando para um arquivo CSV
$users | Export-Csv -Path "C:\Data\new_users.csv" -NoTypeInformation
```

Após a execução, um arquivo chamado `new_users.csv` é criado com os dados fornecidos.

### Filtrando e Manipulando o Conteúdo do CSV

Para filtrar ou manipular os dados de um arquivo CSV, use as capacidades de manipulação de objetos do PowerShell. Por exemplo, para selecionar apenas usuários acima de uma certa idade e de uma cidade específica:

```powershell
# Importando e filtrando dados
$filteredData = Import-Csv -Path "C:\Data\users.csv" | Where-Object {
    $_.Age -gt 25 -and $_.City -eq 'Los Angeles'
}

# Exibindo dados filtrados
$filteredData
```

**Saída de Exemplo:**

```
Name    Age    City
----    ---    ----
Doe     29     Los Angeles
```

### Usando Bibliotecas de Terceiros

Embora os cmdlets nativos do PowerShell sejam geralmente suficientes para tarefas comuns, operações mais complexas podem se beneficiar de bibliotecas ou ferramentas de terceiros. No entanto, para manipulação padrão de CSV, como leitura, escrita, filtragem ou ordenação, os cmdlets integrados do PowerShell como `Import-Csv` e `Export-Csv` usualmente oferecem funcionalidade robusta sem a necessidade de bibliotecas adicionais.
