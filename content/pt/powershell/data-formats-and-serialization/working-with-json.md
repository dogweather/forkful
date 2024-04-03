---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:23:26.600595-07:00
description: 'Como fazer: #.'
lastmod: '2024-03-13T22:44:46.818169-06:00'
model: gpt-4-0125-preview
summary: '#.'
title: Trabalhando com JSON
weight: 38
---

## Como fazer:


### Analisando JSON
Para ler ou analisar JSON no PowerShell, você pode usar o cmdlet `ConvertFrom-Json`. Dada uma string JSON, este cmdlet a converte em um objeto PowerShell.

```powershell
$json = '{"name": "John Doe", "age": 30, "city": "New York"}'
$person = $json | ConvertFrom-Json
$person.name
```

Saída de exemplo:

```
John Doe
```

Este exemplo demonstra como analisar uma simples string JSON para acessar propriedades do objeto resultante.

### Gerando JSON
Para gerar JSON de um objeto PowerShell, você pode usar o cmdlet `ConvertTo-Json`. Isso é útil para preparar dados a serem enviados para um serviço web ou salvos em um arquivo de configuração.

```powershell
$person = [PSCustomObject]@{
    name = "Jane Doe"
    age = 25
    city = "Los Angeles"
}
$json = $person | ConvertTo-Json
Write-Output $json
```

Saída de exemplo:

```json
{
    "name":  "Jane Doe",
    "age":  25,
    "city":  "Los Angeles"
}
```

Este trecho de código cria um objeto PowerShell e então o converte para uma string JSON.
