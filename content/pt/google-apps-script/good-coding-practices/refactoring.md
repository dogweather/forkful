---
title:                "Refatoração"
aliases:
- /pt/google-apps-script/refactoring.md
date:                  2024-02-01T21:59:38.585469-07:00
model:                 gpt-4-0125-preview
simple_title:         "Refatoração"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/google-apps-script/refactoring.md"
changelog:
  - 2024-02-01, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## O Quê & Por Quê?

Refatoração, no léxico de programação, refere-se ao processo de reestruturar código de computador já existente — mudando a fatoração sem alterar seu comportamento externo — para melhorar atributos não funcionais. É um passo vital para os programadores aprimorarem a legibilidade do código, reduzirem a complexidade e, potencialmente, descobrirem bugs latentes, fomentando uma manutenção mais fácil e escalabilidade de código futuro.

## Como Fazer:

No Google Apps Script, um cenário comum que se beneficia da refatoração é a simplificação de scripts pesados que interagem com o Google Sheets ou Docs. Inicialmente, os scripts podem ser escritos de maneira rápida e improvisada para obter resultados rapidamente. Com o tempo, à medida que o script cresce, ele se torna difícil de manejar. Vamos acompanhar um exemplo de refatoração para melhor legibilidade e eficiência.

**Script Original:**

```javascript
function logSheetNames() {
  var sheets = SpreadsheetApp.getActiveSpreadsheet().getSheets();
  for (var i = 0; i < sheets.length; i++) {
    Logger.log(sheets[i].getName());
  }
}
```

Esta função registra o nome de cada planilha em um Google Spreadsheet. Embora funcione bem, ela emprega práticas de JavaScript desatualizadas e falta clareza.

**Script Refatorado:**

```javascript
function logSheetNames() {
  const sheets = SpreadsheetApp.getActiveSpreadsheet().getSheets();
  sheets.forEach(sheet => Logger.log(sheet.getName()));
}
```

Na versão refatorada, mudamos para usar `const` para variáveis que não mudam, tornando nossa intenção mais clara. Também utilizamos o método `forEach`, uma abordagem mais moderna e concisa para iterar sobre arrays, melhorando a legibilidade.

**Saída de Amostra (para ambos scripts):**

A saída no Logger parecerá algo assim, assumindo que seu documento do Google Sheets tem duas planilhas chamadas "Despesas" e "Receita":

```
[20-04-2023 10:00:00: INFO] Despesas
[20-04-2023 10:00:01: INFO] Receita
```

O script refatorado alcança o mesmo resultado, mas é mais limpo e fácil de entender à primeira vista.

## Aprofundamento

A refatoração no Google Apps Script herda parcialmente seus princípios da prática mais ampla de engenharia de software. Tornou-se mais reconhecida e estruturada como um conceito no final dos anos 1990, notavelmente devido ao livro seminal de Martin Fowler "Refatoração: Aperfeiçoando o Projeto de Códigos Existentes" (1999), que forneceu um guia abrangente para várias técnicas de refatoração. Embora os detalhes da refatoração possam variar entre linguagens de programação devido às suas diferenças sintáticas e funcionais, o objetivo central permanece o mesmo: melhorar o código sem alterar seu comportamento externo.

No contexto do Google Apps Script, um aspecto importante a considerar durante a refatoração são as cotas de serviços e limitações impostas pelo Google. Código refatorado eficientemente não só é mais legível, mas também executa mais rápido e de maneira mais confiável dentro dessas restrições. Por exemplo, operações em lote (`Range.setValues()` em vez de definir valores uma célula de cada vez) podem reduzir significativamente o tempo de execução e o consumo de cota.

É importante notar, no entanto, que para certos projetos complexos, o Google Apps Script pode ficar aquém devido a essas limitações. Nesses casos, procurar alternativas como o Google Cloud Functions ou o irmão mais novo do Apps Script, o AppSheet, pode oferecer uma melhor escalabilidade e funcionalidade.

Em última análise, enquanto a refatoração é uma habilidade crítica na manutenção e melhoria de projetos do Google Apps Script, entender as limitações do ambiente e considerar soluções alternativas é igualmente importante para entregar um código eficiente, robusto e sustentável.
