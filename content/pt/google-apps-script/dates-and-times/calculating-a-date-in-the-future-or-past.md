---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 21:49:01.827437-07:00
description: "Calcular uma data no futuro ou no passado trata-se de manipular objetos\
  \ de data para encontrar datas al\xE9m ou antes da data presente, respectivamente.\u2026"
lastmod: '2024-02-25T18:49:43.786038-07:00'
model: gpt-4-0125-preview
summary: "Calcular uma data no futuro ou no passado trata-se de manipular objetos\
  \ de data para encontrar datas al\xE9m ou antes da data presente, respectivamente.\u2026"
title: Calculando uma data no futuro ou no passado
---

{{< edit_this_page >}}

## O Que & Por Que?

Calcular uma data no futuro ou no passado trata-se de manipular objetos de data para encontrar datas além ou antes da data presente, respectivamente. Programadores fazem isso para tarefas que vão desde configurar lembretes e datas de expiração até analisar tendências de dados baseadas em tempo.

## Como fazer:

No Google Apps Script, que é baseado em JavaScript, você pode manipular datas usando o objeto `Date`. Aqui está como calcular datas no futuro e no passado:

### Cálculo de Data Futura

Para calcular uma data futura, você cria um objeto de data para a data atual e depois adiciona o número desejado de dias (ou outra unidade de tempo) a ele.

```javascript
// Data atual
var today = new Date();

// Calcular uma data 10 dias no futuro
var futureDate = new Date(today);
futureDate.setDate(today.getDate() + 10);

Logger.log("Data Futura: " + futureDate.toDateString());
```

### Cálculo de Data Passada

De forma similar, para encontrar uma data no passado, subtraia o número de dias da data atual.

```javascript
// Data atual
var today = new Date();

// Calcular uma data 10 dias no passado
var pastDate = new Date(today);
pastDate.setDate(today.getDate() - 10);

Logger.log("Data Passada: " + pastDate.toDateString());
```

### Exemplo de Saída

Isso produziria algo como o seguinte (assumindo que hoje é 15 de abril de 2023):

```
Data Futura: Ter Abr 25 2023
Data Passada: Qua Abr 05 2023
```

Lembre-se, o objeto Date em JavaScript (e, consequentemente, no Google Apps Script) ajusta automaticamente os meses e anos conforme você adiciona ou subtrai dias.

## Aprofundamento

A manipulação de datas usando o objeto `Date` tem origem nas primeiras implementações de JavaScript. Com o tempo, essa abordagem tem permanecido geralmente consistente, oferecendo uma maneira direta para os desenvolvedores gerenciarem datas sem precisar de bibliotecas externas. No entanto, para operações mais complexas como ajustes de fuso horário, ou ao trabalhar com dados extensivos baseados em datas, bibliotecas como `Moment.js` ou o mais moderno `Luxon` podem oferecer mais funcionalidades e um manuseio mais fácil.

No Google Apps Script, especificamente, apesar da disponibilidade direta e simplicidade do objeto `Date`, é crucial estar atento a como os cálculos de datas podem impactar o desempenho do script e o tempo de execução, especialmente em gatilhos acionados pelo tempo ou manipulações extensivas de planilhas. Além disso, embora o Google Apps Script forneça métodos integrados para lidar com datas dentro de seu ecossistema (como no Google Sheets ou Calendar), integrar bibliotecas externas ou aproveitar os Serviços Avançados do Google pode, às vezes, oferecer soluções mais robustas para cenários complexos.

Assim, enquanto a metodologia nativa do objeto JavaScript `Date` geralmente é suficiente para cálculos diretos, explorar bibliotecas externas ou serviços pode aprimorar a funcionalidade para requisitos mais matizados.
