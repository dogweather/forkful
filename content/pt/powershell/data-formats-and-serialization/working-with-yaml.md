---
aliases:
- /pt/powershell/working-with-yaml/
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:26:22.901371-07:00
description: "YAML, ou YAML Ain't Markup Language, \xE9 uma linguagem de serializa\xE7\
  \xE3o de dados leg\xEDvel por humanos. Programadores frequentemente a utilizam para\
  \ arquivos\u2026"
lastmod: 2024-02-18 23:08:58.388952
model: gpt-4-0125-preview
summary: "YAML, ou YAML Ain't Markup Language, \xE9 uma linguagem de serializa\xE7\
  \xE3o de dados leg\xEDvel por humanos. Programadores frequentemente a utilizam para\
  \ arquivos\u2026"
title: Trabalhando com YAML
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
