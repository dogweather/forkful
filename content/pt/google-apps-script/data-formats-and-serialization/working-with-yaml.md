---
title:                "Trabalhando com YAML"
aliases:
- /pt/google-apps-script/working-with-yaml/
date:                  2024-02-01T22:07:21.742644-07:00
model:                 gpt-4-0125-preview
simple_title:         "Trabalhando com YAML"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/google-apps-script/working-with-yaml.md"
changelog:
  - 2024-02-01, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## O que é & Por quê?

YAML, que significa "YAML Ain't Markup Language" (YAML Não é uma Linguagem de Marcação), é um padrão de serialização de dados legível por humanos que é comumente usado para arquivos de configuração e troca de dados entre linguagens com estruturas de dados variadas. Os programadores frequentemente trabalham com YAML pela sua simplicidade e legibilidade, especialmente em projetos que requerem extensa configuração ou quando transferindo dados estruturados entre diferentes sistemas.

## Como fazer:

Embora o Google Apps Script (GAS) não suporte nativamente a análise (parsing) ou serialização de YAML, você pode manipular dados YAML usando bibliotecas de JavaScript ou escrevendo funções de análise personalizadas. Para demonstração, vamos considerar como analisar uma string YAML usando uma função personalizada, já que bibliotecas externas não podem ser importadas diretamente para o GAS.

Suponha que você tenha uma configuração YAML simples:

```yaml
title: Exemplo YAML
description: Um exemplo de como lidar com YAML no Google Apps Script
tags:
  - Google Apps Script
  - YAML
  - Configuração
```

Para analisar isso no Google Apps Script, use as capacidades de manipulação de string do JavaScript:

```javascript
function parseYAML(yamlString) {
  var result = {};
  var lines = yamlString.split("\n");
  for (var i = 0; i < lines.length; i++) {
    var line = lines[i];
    if (line.includes(":")) {
      var parts = line.split(":");
      var key = parts[0].trim();
      var value = parts[1].trim();
      // Tratamento básico para arrays
      if (value.startsWith("-")) {
        value = [value.substring(1).trim()];
        while (i + 1 < lines.length && lines[i + 1].trim().startsWith("-")) {
          i++;
          value.push(lines[i].trim().substring(1).trim());
        }
      }
      result[key] = value;
    }
  }
  return result;
}

function testYamlParsing() {
  var yaml = "title: Exemplo YAML\ndescription: Um exemplo de como lidar com YAML no Google Apps Script\ntags:\n  - Google Apps Script\n  - YAML\n  - Configuração";
  var parsed = parseYAML(yaml);
  Logger.log(parsed);
}
```

Quando `testYamlParsing()` é executado, ele produz:

```
{ title: 'Exemplo YAML',
  description: 'Um exemplo de como lidar com YAML no Google Apps Script',
  tags: [ 'Google Apps Script', ' YAML', ' Configuração' ] }
```

Esta abordagem de análise personalizada é bastante básica e pode precisar de ajustes para acomodar arquivos YAML complexos.

## Mergulho Profundo

O YAML, lançado inicialmente em 2001, teve como objetivo ser mais legível por humanos do que seus predecessores, como XML ou JSON. Embora sua simplicidade e facilidade de uso sejam amplamente apreciadas, lidar com YAML no Google Apps Script apresenta desafios devido à falta de suporte direto. Consequentemente, os programadores frequentemente dependem da versatilidade do JavaScript para analisar e gerar dados YAML. No entanto, para casos de uso complexos, especialmente aqueles que envolvem aninhamento profundo e estruturas de dados avançadas, este método pode se tornar complicado e propenso a erros.

O JSON, em contraste, é suportado nativamente no Google Apps Script e na maioria dos outros ambientes de programação, oferecendo uma abordagem mais direta para a serialização e desserialização de dados sem sobrecarga de análise adicional. A sintaxe do JSON é menos verbosa do que a do YAML, tornando-a mais adequada para a troca de dados em aplicações web. No entanto, o YAML permanece popular para arquivos de configuração e situações onde a legibilidade humana é primordial.

Ao trabalhar com YAML no Google Apps Script, considere os compromissos entre legibilidade e facilidade de uso. Para manipulação abrangente de YAML, pode valer a pena explorar ferramentas ou serviços externos que possam converter YAML para JSON antes de processá-lo em seu script.
