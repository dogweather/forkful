---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 21:48:41.513791-07:00
description: "Calcular uma data no futuro ou passado envolve determinar uma data que\
  \ est\xE1 um n\xFAmero especificado de dias, meses ou anos distante de uma data\
  \ dada.\u2026"
lastmod: '2024-03-13T22:44:46.426928-06:00'
model: gpt-4-0125-preview
summary: "Calcular uma data no futuro ou passado envolve determinar uma data que est\xE1\
  \ um n\xFAmero especificado de dias, meses ou anos distante de uma data dada."
title: Calculando uma data no futuro ou no passado
weight: 26
---

## Como fazer:
Em Visual Basic for Applications (VBA), a função primária usada para calcular datas futuras ou passadas é `DateAdd()`. Esta função adiciona um intervalo de tempo especificado a uma data, retornando uma nova data.

Aqui está um exemplo básico para adicionar 10 dias à data atual:

```vb
Dim futureDate As Date
futureDate = DateAdd("d", 10, Date) ' Adiciona 10 dias à data atual
Debug.Print futureDate ' Produz algo como: 20/04/2023
```

Similarmente, para encontrar uma data 10 dias no passado:

```vb
Dim pastDate As Date
pastDate = DateAdd("d", -10, Date) ' Subtrai 10 dias da data atual
Debug.Print pastDate ' Produz: 31/03/2023, assumindo que hoje seja 10/04/2023
```

Esses exemplos são bastante diretos. Você pode substituir `"d"` por outros códigos de intervalo, como `"m"` para meses e `"yyyy"` para anos, para calcular diferentes tipos de cálculos de datas. Aqui está como você poderia calcular uma data um ano no futuro:

```vb
Dim nextYear As Date
nextYear = DateAdd("yyyy", 1, Date) ' Adiciona 1 ano à data atual
Debug.Print nextYear ' Produz: 10/04/2024 se hoje é 10/04/2023
```

## Aprofundamento
A função `DateAdd` tem sido uma parte fundamental do VBA desde o seu início, derivando do seu predecessor BASIC. Embora ofereça simplicidade para adicionar ou subtrair intervalos de tempo de datas, é vital observar que o VBA, incluindo suas funções de manipulação de data, pode não sempre corresponder à conveniência ou eficiência encontradas em linguagens de programação mais modernas.

Por exemplo, linguagens modernas como Python com o módulo `datetime` ou JavaScript com bibliotecas como `moment.js` e `date-fns` fornecem maneiras mais intuitivas e poderosas para manipulação de datas. Estas opções oferecem melhor suporte para localização, fusos horários e anos bissextos, o que pode torná-las mais adequadas para aplicações que requerem cálculos de datas precisos em escala global.

No entanto, para macros do Excel e aplicações que requerem integração dentro do ecossistema da Microsoft Office, o VBA permanece uma escolha prática. A simplicidade no acesso direto e manipulação de dados do Excel é uma vantagem significativa. Além do mais, para a maioria dos cálculos de datas básicos como agendamento e lembretes, `DateAdd()` em VBA fornece uma solução adequada e direta. Sua sintaxe é fácil de entender para os recém-chegados, enquanto sua integração nas aplicações mais amplas da suíte Office garante sua relevância em casos de uso específicos.

Em conclusão, embora linguagens de programação alternativas possam oferecer abordagens mais modernas para o cálculo de datas, `DateAdd()` em VBA serve como um testemunho do poder de permanência da linguagem nos domínios onde é mais necessária.
