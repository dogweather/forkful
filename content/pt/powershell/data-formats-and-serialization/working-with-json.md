---
title:                "Trabalhando com JSON"
aliases:
- /pt/powershell/working-with-json.md
date:                  2024-02-03T19:23:26.600595-07:00
model:                 gpt-4-0125-preview
simple_title:         "Trabalhando com JSON"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/powershell/working-with-json.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## O Que & Por Quê?

A integração do PowerShell com JSON (JavaScript Object Notation) trata da análise (leitura) e geração (escrita) de dados JSON, um formato comum para troca de dados na web. Os programadores trabalham com JSON para interagir com APIs web, arquivos de configuração ou para facilitar a troca de dados entre diferentes linguagens e plataformas devido à sua natureza leve e independente de linguagem.

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
