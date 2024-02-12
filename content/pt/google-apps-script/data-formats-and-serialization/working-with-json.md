---
title:                "Trabalhando com JSON"
aliases:
- /pt/google-apps-script/working-with-json.md
date:                  2024-02-01T22:05:38.383662-07:00
model:                 gpt-4-0125-preview
simple_title:         "Trabalhando com JSON"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/google-apps-script/working-with-json.md"
changelog:
  - 2024-02-01, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## O Quê e Por Quê?

JSON, ou Notação de Objeto JavaScript, é um formato leve para armazenar e transportar dados, ideal para comunicação de servidor para cliente e arquivos de configuração. Programadores aproveitam-no no Google Apps Script para troca de dados sem problemas entre os serviços do Google (como Sheets, Docs, Drive) e fontes externas, devido à sua estrutura legível por humanos e fácil integração em ambientes baseados em JavaScript.

## Como fazer:

No Google Apps Script, manipular JSON é um processo direto, em grande parte devido ao suporte nativo que o JavaScript oferece para análise (parsing) e serialização (stringification) de JSON. Aqui estão algumas operações comuns:

**1. Análise de JSON**: Suponha que recuperamos uma string JSON de um serviço web; analisá-la para um objeto JavaScript é essencial para a manipulação de dados.

```javascript
var jsonString = '{"name": "Sample Project", "version": "1.0.0"}';
var obj = JSON.parse(jsonString);
Logger.log(obj.name); // Saída: Sample Project
```

**2. Serialização de Objetos JavaScript**: Inversamente, converter um objeto JavaScript para uma string JSON é útil quando precisamos enviar dados do Apps Script para um serviço externo.

```javascript
var projectData = {
  name: "Sample Project",
  version: "1.0.0"
};
var jsonString = JSON.stringify(projectData);
Logger.log(jsonString); // Saída: '{"name":"Sample Project","version":"1.0.0"}'
```

**3. Trabalhando com Dados Complexos**:
Para estruturas de dados mais complexas, como arrays de objetos, o processo permanece o mesmo, mostrando a flexibilidade do JSON para representação de dados.

```javascript
var projects = [
  {name: "Project 1", version: "1.0"},
  {name: "Project 2", version: "2.0"}
];
var jsonString = JSON.stringify(projects);
Logger.log(jsonString); // Saída: '[{"name":"Project 1","version":"1.0"},{"name":"Project 2","version":"2.0"}]'
```

## Aprofundamento

A ubiquidade do JSON em aplicações web modernas não pode ser subestimada, enraizada em sua simplicidade e como se integra perfeitamente com JavaScript, a linguagem da web. Seu design, inspirado em literais de objeto JavaScript, embora mais estrito, facilita sua adoção rápida. No início dos anos 2000, JSON ganhou popularidade como uma alternativa ao XML para aplicações web impulsionadas por AJAX, oferecendo um formato de intercâmbio de dados mais leve e menos verboso. Dada a integração profunda do Google Apps Script com várias APIs do Google e serviços externos, JSON serve como um formato crucial para estruturar, transportar e manipular dados nessas plataformas.

Enquanto JSON reina supremo para aplicações web, formatos de dados alternativos como YAML para arquivos de configuração ou Protobuf para serialização binária mais eficiente em ambientes de alta performance existem. No entanto, o equilíbrio do JSON de legibilidade, facilidade de uso e amplo suporte em linguagens de programação e ferramentas consolida sua posição como escolha padrão para muitos desenvolvedores aventurando-se no Google Apps Script e além.
