---
title:                "Trabalhando com YAML"
html_title:           "Arduino: Trabalhando com YAML"
simple_title:         "Trabalhando com YAML"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/javascript/working-with-yaml.md"
---

{{< edit_this_page >}}

## What & Why?
Trabalhar com YAML envolve manipular dados em um formato legível por humanos, muito usado em arquivos de configuração. Programadores usam YAML por sua facilidade de leitura e escrita, além de ser comum em projetos com Docker, Kubernetes, e CI/CD.

## How to:
Para trabalhar com YAML em JavaScript, vamos converter YAML para JSON e vice-versa usando a biblioteca `js-yaml`.

```javascript
const yaml = require('js-yaml');
const fs = require('fs');

// Carregando YAML de um arquivo e convertendo para JSON
try {
  const doc = yaml.load(fs.readFileSync('config.yml', 'utf8'));
  console.log(doc);
} catch (e) {
  console.error(e);
}

// Convertendo JSON para YAML e salvando em um arquivo
const data = { titulo: "Exemplo", versao: 1.0 };
try {
  const yamlContent = yaml.dump(data);
  fs.writeFileSync('config.yml', yamlContent, 'utf8');
  console.log('YAML salvo com sucesso!');
} catch (e) {
  console.error(e);
}
```

## Deep Dive
YAML, que significa "YAML Ain't Markup Language", é um formato de serialização de dados projetado para ser legível por humanos. Surgiu em 2001 como alternativa ao XML e outros formatos de serialização mais complexos. JSON é uma alternativa direta ao YAML, sendo mais apto para dados complexos ou quando o desempenho é crítico. Na implementação, é importante usar uma biblioteca confiável, já que trabalhar com YAML envolve a interpretação correta das indentações e símbolos como `---` para separar documentos dentro de um mesmo arquivo.

## See Also
- Documentação `js-yaml`: https://www.npmjs.com/package/js-yaml
- Especificação YAML: https://yaml.org/spec/1.2/spec.html
- Tutorial YAML para iniciantes: https://www.cloudbees.com/blog/yaml-tutorial-everything-you-need-get-started
