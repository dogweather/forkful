---
title:                "Analisando uma data a partir de uma string"
aliases:
- pt/google-apps-script/parsing-a-date-from-a-string.md
date:                  2024-02-01T21:57:18.481011-07:00
model:                 gpt-4-0125-preview
simple_title:         "Analisando uma data a partir de uma string"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/google-apps-script/parsing-a-date-from-a-string.md"
changelog:
  - 2024-02-01, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## O que & Por quê?

Analisar uma data a partir de uma string envolve converter um texto que representa uma data em um objeto de data, permitindo que programadores realizem operações relacionadas a datas, como comparações, aritmética e formatação. É essencial para tratar entradas do usuário, processar dados de fontes externas e gerenciar datas em vários formatos, especialmente em aplicações que envolvem agendamento, análise de dados ou qualquer forma de registros baseados em tempo.

## Como fazer:

No Google Apps Script, que é baseado em JavaScript, você tem várias abordagens para analisar uma data a partir de uma string. Abaixo estão exemplos usando tanto métodos nativos de JavaScript quanto utilidades do Google Apps Script.

**Usando o construtor `new Date()`:**

A maneira mais simples de analisar uma string em uma data no Google Apps Script é usando o construtor do objeto `Date`. No entanto, isso requer que a string da data esteja em um formato reconhecido pelo método Date.parse() (por exemplo, AAAA-MM-DD).

```javascript
const dateString = '2023-04-01';
const dateObject = new Date(dateString);
Logger.log(dateObject); // Registra Sat Apr 01 2023 00:00:00 GMT+0000 (UTC)
```

**Usando `Utilities.parseDate()`:**

Para mais flexibilidade, particularmente com formatos de data personalizados, Google Apps Script fornece `Utilities.parseDate()`. Este método permite que você especifique o formato da data, fuso horário e localidade.

```javascript
const dateString = '01-04-2023'; // DD-MM-AAAA
const format = 'dd-MM-yyyy';
const timezone = Session.getScriptTimeZone();
const dateObject = Utilities.parseDate(dateString, timezone, format);
Logger.log(dateObject); // Registra Sat Apr 01 2023 00:00:00 GMT+0000 (UTC) dependendo do fuso horário do script
```

Nota: Enquanto `Utilities.parseDate()` oferece mais controle, seu comportamento pode variar com base no fuso horário do script, então é crucial especificar explicitamente o fuso horário se sua aplicação lida com datas em várias regiões.

## Aprofundando

A análise de datas em linguagens de programação tem sido historicamente repleta de desafios, principalmente devido à variedade de formatos de datas e às complexidades dos fusos horários. A abordagem do Google Apps Script, principalmente derivada do JavaScript, visa simplificar isso oferecendo tanto o objeto `Date` direto quanto a função mais versátil `Utilities.parseDate()`. No entanto, cada método tem suas limitações; por exemplo, confiar no construtor `Date` com strings leva a inconsistências em diferentes ambientes devido a interpretações divergentes dos formatos de datas. Por outro lado, `Utilities.parseDate()` exige uma compreensão mais clara do formato, fuso horário e localidade, tornando-o um pouco mais complexo, mas mais confiável para necessidades específicas.

Bibliotecas ou serviços alternativos, como Moment.js (agora recomendando Luxon para novos projetos), fornecem funcionalidades mais ricas e melhor tratamento de zonas, abordando muitos desses desafios. Ainda assim, no contexto do Google Apps Script, onde bibliotecas externas têm limitações, entender e aproveitar os métodos internos efetivamente se torna crucial. Programadores vindo de outras linguagens podem achar as nuances do tratamento de datas no Google Apps Script desafiadoras de forma única, mas podem alcançar uma análise de datas robusta com um profundo entendimento das ferramentas disponíveis e consideração cuidadosa da natureza global de suas aplicações.
