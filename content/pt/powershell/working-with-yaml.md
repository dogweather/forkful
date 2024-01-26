---
title:                "Trabalhando com YAML"
html_title:           "Arduino: Trabalhando com YAML"
simple_title:         "Trabalhando com YAML"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/powershell/working-with-yaml.md"
---

{{< edit_this_page >}}

## What & Why?
Trabalhar com YAML significa manipular dados estruturados similares ao JSON, mas com ênfase na legibilidade por humanos. Programadores utilizam YAML frequentemente para configuração de aplicações, automação e integração contínua devido à sua clareza e simplicidade.

## How to:

### Ler um arquivo YAML:
```PowerShell
# Instale o módulo necessário
Install-Module -Name powershell-yaml

# Importe o módulo
Import-Module powershell-yaml

# Carregue o conteúdo do arquivo
$conteudoYaml = Get-Content -Path 'caminho/do/arquivo.yaml' -Raw

# Converta o conteúdo para um objeto PowerShell
$objetoYaml = ConvertFrom-Yaml -Yaml $conteudoYaml

# Acesse os dados
$objetoYaml.chave
```

### Escrever um arquivo YAML:
```PowerShell
# Crie um objeto
$info = @{
   nome = "João"
   idade = 27
}

# Converta o objeto para YAML
$conteudoYaml = ConvertTo-Yaml -Data $info

# Salve o YAML em um arquivo
$conteudoYaml | Out-File -FilePath 'caminho/do/arquivo.yaml'
```

### Output de exemplo (arquivo 'configuracoes.yaml'):
```yaml
nome: João
idade: 27
```

## Deep Dive

YAML, que significa "YAML Ain't Markup Language", é uma linguagem de serialização de dados humanamente legível, ideal para configuração de software e transmissão de dados entre diferentes linguagens de programação. Surgiu em 2001 como uma alternativa ao XML e seu nome original era "Yet Another Markup Language", mudando depois para destacar seu design legível por humanos. Alguns formatos alternativos são o JSON, mais conciso e frequentemente usado em APIs web, e o TOML, projetado como um formato de configuração mínima.

Como detalhes de implementação, ao trabalhar com YAML no PowerShell, o módulo `powershell-yaml` é amplamente usado, pois fornece funcionalidades de conversão entre YAML e objetos do PowerShell. É importante manter a indentação correta ao escrever YAML, visto que a estrutura é baseada nessa indentação e não no uso de delimitadores como chaves ou colchetes.

## See Also

Para aprimorar suas habilidades com YAML e PowerShell, consulte:

- Documentação oficial do módulo `powershell-yaml`: [PowerShell Gallery | powershell-yaml](https://www.powershellgallery.com/packages/powershell-yaml)
- Guia de sintaxe YAML: [YAML Syntax](https://learnxinyminutes.com/docs/yaml/)
