---
title:                "Obtendo a data atual"
aliases:
- /pt/typescript/getting-the-current-date/
date:                  2024-02-03T19:11:09.273195-07:00
model:                 gpt-4-0125-preview
simple_title:         "Obtendo a data atual"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/typescript/getting-the-current-date.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## O Que e Por Quê?
Obter a data atual em TypeScript, uma linguagem construída sobre o JavaScript, permite acessar e manipular as informações da data e hora atuais. Programadores frequentemente precisam dessa funcionalidade para criar registros de data e hora, agendamentos e outras características sensíveis ao tempo em suas aplicações.

## Como fazer:
No TypeScript, você pode usar o objeto `Date` para obter a data e a hora atuais. Veja como você pode fazer isso:

```typescript
const currentDate = new Date();
console.log(currentDate);
```

Saída de exemplo:
```
2023-04-12T07:20:50.52Z
```

Este trecho de código cria um novo objeto `Date` contendo a data e a hora atuais, que é então impresso no console. Você também pode formatar a data usando toLocaleDateString() para formatos mais legíveis:

```typescript
const currentDate = new Date();
console.log(currentDate.toLocaleDateString());
```

Saída de exemplo:
```
12/04/2023
```

### Usando date-fns
Para manipulação e formatação de datas mais extensas, a biblioteca `date-fns` é uma escolha popular. Primeiro, instale-a via npm:

```bash
npm install date-fns
```

Depois, você pode usá-la para formatar a data atual:

```typescript
import { format } from 'date-fns';

const currentDate = new Date();
console.log(format(currentDate, 'yyyy-MM-dd'));
```

Saída de exemplo:
```
2023-04-12
```

Este exemplo usando `date-fns` formata a data atual como uma string no formato "YYYY-MM-DD". A biblioteca oferece uma infinidade de funções para manipulação de datas, tornando-a uma ferramenta versátil para qualquer programador TypeScript que trabalhe com datas.
