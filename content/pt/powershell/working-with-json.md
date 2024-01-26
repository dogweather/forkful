---
title:                "Trabalhando com JSON"
html_title:           "Arduino: Trabalhando com JSON"
simple_title:         "Trabalhando com JSON"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/powershell/working-with-json.md"
---

{{< edit_this_page >}}

## O Que É & Por Que Usar?
Trabalhar com JSON significa manipular JavaScript Object Notation, um formato leve para intercâmbio de dados. Programadores recorrem ao JSON porque ele é fácil de ler e escrever, além de ser amplamente suportado em várias linguagens, facilitando o compartilhamento de dados entre sistemas.

## Como Fazer:
Para trabalhar com JSON no PowerShell, você geralmente usará os cmdlets `ConvertFrom-Json` e `ConvertTo-Json`. Aqui estão alguns exemplos para te guiar:

```PowerShell
# Converter uma string JSON para um objeto PowerShell
$jsonString = '{"nome": "João", "idade": 31}'
$objeto = $jsonString | ConvertFrom-Json
$objeto.nome  # Saída: João

# Converter um objeto PowerShell para uma string JSON
$pessoa = [PSCustomObject]@{
    nome = "Ana"
    idade = 27
}
$jsonConvertido = $pessoa | ConvertTo-Json
$jsonConvertido  # Saída: {"nome": "Ana", "idade": 27}
```

## Mergulho Profundo:
JSON surgiu em 2001, proposto por Douglas Crockford, e rapidamente tornou-se popular como formato de intercâmbio de dados na web. Alternativas incluem XML e YAML, mas JSON se destaca pela simplicidade. No PowerShell, trabalhar com JSON se beneficia da integração com o .NET Framework, que possui classes dedicadas à parse e geração de JSON, como `Newtonsoft.Json`.

## Veja Também:
- Documentação oficial do PowerShell para `ConvertFrom-Json`: https://docs.microsoft.com/pt-br/powershell/module/microsoft.powershell.utility/convertfrom-json
- Documentação oficial do PowerShell para `ConvertTo-Json`: https://docs.microsoft.com/pt-br/powershell/module/microsoft.powershell.utility/convertto-json
- Guia rápido JSON: https://www.json.org/json-pt.html
- Tutorial Newtonsoft.Json para .NET: https://www.newtonsoft.com/json
