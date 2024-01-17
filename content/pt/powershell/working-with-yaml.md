---
title:                "Trabalhando com yaml"
html_title:           "PowerShell: Trabalhando com yaml"
simple_title:         "Trabalhando com yaml"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/powershell/working-with-yaml.md"
---

{{< edit_this_page >}}

## O que e por que?

Trabalhar com YAML significa lidar com arquivos de configuração de uma forma simples e fácil de ler e escrever. Programadores muitas vezes usam YAML para armazenar e transmitir dados estruturados em seus projetos, permitindo uma organização mais eficiente e legível do código.

## Como fazer:

Usando o PowerShell, podemos trabalhar com YAML usando o módulo [PSYaml](https://github.com/cloudbase/powershell-yaml). Aqui está um exemplo de como criar um arquivo YAML usando o PowerShell:

```powershell
Import-Module PSYaml
$data = @{
    nome = "João"
    idade = 25
    linguagens = @("PowerShell", "Python", "Javascript")
}
$data | ConvertTo-Yaml | Out-File dados.yaml
```

Para ler e processar um arquivo YAML existente, podemos usar o seguinte código:

```powershell
$dados = Get-Content dados.yaml | ConvertFrom-Yaml
$dados.nome
# Saída: João
```

## Profundando:

O YAML (YAML Ain't Markup Language) foi criado por Clark Evans em 2001 como uma alternativa mais amigável ao XML. Ele é frequentemente usado para configurar projetos, como em ferramentas de automação de infraestrutura como o Ansible ou em aplicações web, como o Docker Compose.

Outra alternativa popular para armazenar e transmitir dados estruturados é o formato JSON, porém YAML oferece uma sintaxe mais legível para humanos, o que facilita a manutenção e o compartilhamento de código entre diferentes equipes e linguagens de programação.

Ao trabalhar com YAML, é importante notar que espaços e indentação são fundamentais para a formatação correta do código. Além disso, é possível incluir comentários no código usando o caractere "#".

## Veja também:

- [Documentação do módulo PSYaml](https://github.com/cloudbase/powershell-yaml/blob/master/README.md)
- [Site oficial do YAML](https://yaml.org/)
- [YAML vs JSON: Qual escolher?](https://www.freecodecamp.org/news/yaml-vs-json-which-is-better-for-configuration-c43fd70cf9db/)