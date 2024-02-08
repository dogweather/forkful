---
title:                "Trabalhando com YAML"
aliases:
- pt/powershell/working-with-yaml.md
date:                  2024-02-03T19:26:22.901371-07:00
model:                 gpt-4-0125-preview
simple_title:         "Trabalhando com YAML"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/powershell/working-with-yaml.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## O Que & Por Quê?
YAML, ou YAML Ain't Markup Language, é uma linguagem de serialização de dados legível por humanos. Programadores frequentemente a utilizam para arquivos de configuração e transmissão de dados entre linguagens. Sua simplicidade e legibilidade a tornam particularmente popular para tarefas que envolvem a configuração de ambientes, aplicações ou serviços onde as configurações são cruciais e devem ser facilmente compreendidas e editadas.

## Como:
Por padrão, o PowerShell não vem com um cmdlet integrado para analisar YAML, mas funciona sem problemas com YAML quando você utiliza o módulo `powershell-yaml` ou converte YAML em um objeto do PowerShell usando `ConvertFrom-Json` em combinação com uma ferramenta como `yq`.

### Usando o Módulo `powershell-yaml`:
Primeiro, instale o módulo:
```PowerShell
Install-Module -Name powershell-yaml
```

Para ler um arquivo YAML:
```PowerShell
Import-Module powershell-yaml
$content = Get-Content -Path 'config.yml' -Raw
$yamlObject = ConvertFrom-Yaml -Yaml $content
Write-Output $yamlObject
```

Para escrever um objeto do PowerShell em um arquivo YAML:
```PowerShell
$myObject = @{
    name = "John Doe"
    age = 30
    languages = @("PowerShell", "Python")
}
$yamlContent = ConvertTo-Yaml -Data $myObject
$yamlContent | Out-File -FilePath 'output.yml'
```

Exemplo de `output.yml`:
```yaml
name: John Doe
age: 30
languages:
- PowerShell
- Python
```

### Analisando YAML com `yq` e `ConvertFrom-Json`:
Outra abordagem envolve usar `yq`, um processador de YAML de linha de comando leve e portátil. `yq` pode converter YAML em JSON, o qual o PowerShell pode analisar nativamente.

Primeiro, garanta que `yq` esteja instalado em seu sistema.
Então execute:
```PowerShell
$yamlToJson = yq e -o=json ./config.yml
$jsonObject = $yamlToJson | ConvertFrom-Json
Write-Output $jsonObject
```

Este método é particularmente útil para usuários que trabalham em ambientes multiplataforma ou preferem usar JSON dentro do PowerShell.
