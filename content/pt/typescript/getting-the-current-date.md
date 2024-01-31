---
title:                "Obtendo a data atual"
date:                  2024-01-20T15:16:55.558140-07:00
html_title:           "Bash: Obtendo a data atual"
simple_title:         "Obtendo a data atual"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/typescript/getting-the-current-date.md"
---

{{< edit_this_page >}}

## O Que & Porquê?
Pegar a data atual é capturar o momento exato em que algo acontece no seu código. Programadores fazem isso para registrar eventos, comparar datas ou controlar prazos.

## Como fazer:
```TypeScript
const agora = new Date();
console.log(agora); // Exemplo de saída: 2023-04-02T15:47:11.446Z
```

Para formatar a data de maneira legível:
```TypeScript
const agora = new Date();
console.log(agora.toLocaleDateString('pt-BR')); // Exemplo de saída: 02/04/2023
```

Exibindo a hora:
```TypeScript
const agora = new Date();
console.log(agora.toLocaleTimeString('pt-BR')); // Exemplo de saída: 15:47:11
```

## Mergulho Profundo
Pegar a data e a hora sempre foi crucial para sistemas de computador. No JavaScript, a raiz do TypeScript, a classe `Date` está disponível desde o início. Alternativas, como as bibliotecas Moment.js ou date-fns, oferecem mais opções e zonas horárias, mas com a evolução do ECMAScript, a API nativa está ficando mais robusta.

Detalhe de implementação: `Date` no JavaScript/TypeScript é baseado na mesma data e hora do sistema, e usa o formato de tempo UTC para representar o momento. Contudo, com `toLocaleDateString` e `toLocaleTimeString`, é possível converter para o fuso horário local facilmente.

## Veja Também
- [MDN Web Docs: Date](https://developer.mozilla.org/pt-BR/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [TypeScript Official Documentation](https://www.typescriptlang.org/docs/)
- [date-fns Documentation](https://date-fns.org/v2.28.0/docs/Getting-Started)
