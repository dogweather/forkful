---
date: 2024-01-26 04:36:45.722434-07:00
description: "Como fazer: O XML, ou Linguagem de Marca\xE7\xE3o Extens\xEDvel, existe\
  \ desde o final dos anos 90. Sua natureza auto-descritiva e formato leg\xEDvel por\
  \ humanos o\u2026"
lastmod: '2024-04-05T21:53:46.681939-06:00'
model: gpt-4-0125-preview
summary: "O XML, ou Linguagem de Marca\xE7\xE3o Extens\xEDvel, existe desde o final\
  \ dos anos 90."
title: Trabalhando com XML
weight: 40
---

## Como fazer:
```TypeScript
import { parseString } from 'xml2js';

// XML de exemplo
const xml = `<note>
                <to>User</to>
                <from>Author</from>
                <heading>Lembrete</heading>
                <body>Não esqueça a reunião!</body>
             </note>`;

// Analisar XML para JSON
parseString(xml, (err, result) => {
    if(err) throw err;
    console.log(result);
});

// Assumindo que a análise foi bem-sucedida, a saída pode parecer com:
// { note:
//    { to: ['User'],
//      from: ['Author'],
//      heading: ['Lembrete'],
//      body: ['Não esqueça a reunião!'] } 
}
```

## Aprofundamento
O XML, ou Linguagem de Marcação Extensível, existe desde o final dos anos 90. Sua natureza auto-descritiva e formato legível por humanos o tornaram popular no início para várias aplicações como feeds RSS, gerenciamento de configuração e até formatos de documentos de escritório como o Microsoft Office Open XML. Mas, ele é verboso comparado ao JSON, e a maré mudou. O JSON ganhou os holofotes para APIs baseadas na web devido ao seu peso mais leve e compatibilidade nativa com JavaScript.

No entanto, o XML não morreu. Ele é usado em sistemas empresariais de grande escala e para padrões de documentos que não migraram para o JSON. Ferramentas como `xml2js` para TypeScript ou `lxml` em Python provam que ainda há uma necessidade contínua de manipulação de XML na programação.

O TypeScript não possui suporte nativo para XML como tem para JSON. Em vez disso, você trabalha com bibliotecas. `xml2js` é um exemplo. Ele transforma XML em JSON, tornando os dados mais fáceis de serem trabalhados por gurus de JavaScript.

## Veja Também
- [MDN Web Docs sobre XML](https://developer.mozilla.org/en-US/docs/Web/XML/XML_introduction)
- [Pacote npm xml2js](https://www.npmjs.com/package/xml2js)
- [Tutorial de XML do W3Schools](https://www.w3schools.com/xml/)
