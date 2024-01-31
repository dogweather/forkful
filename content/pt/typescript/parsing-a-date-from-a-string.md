---
title:                "Analisando uma data a partir de uma string"
date:                  2024-01-20T15:38:58.637798-07:00
html_title:           "Arduino: Analisando uma data a partir de uma string"
simple_title:         "Analisando uma data a partir de uma string"

category:             "TypeScript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/typescript/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## What & Why?
Converter uma data de uma string significa transformar o texto que representa uma data ("01/01/2023", por exemplo) em um objeto de data que o TypeScript pode entender e manipular. Programadores fazem isso porque as datas frequentemente chegam como strings através de APIs ou entradas de usuário e precisam ser tratadas para cálculos ou armazenagem eficiente.

## How to:
```TypeScript
const dateString: string = '01-01-2023';
const parsedDate: Date = new Date(dateString);

console.log(parsedDate);
// Saída (dependendo do fuso horário): 2023-01-01T00:00:00.000Z
```

Para formatos de data mais complexos, você pode usar bibliotecas como `date-fns` ou `moment.js`:
```TypeScript
import { parse } from 'date-fns';

const dateString: string = '01 de janeiro de 2023 14:00:00';
const dateFormat: string = 'dd 'de' MMMM 'de' yyyy HH:mm:ss';
const parsedDate: Date = parse(dateString, dateFormat, new Date());

console.log(parsedDate);
// Saída: 2023-01-01T14:00:00.000Z
```

## Deep Dive
O TypeScript herdou suas funções de data do JavaScript e, portanto, possui as mesmas idiossincrasias históricas, como meses que começam do zero (janeiro é 0). No passado, muitos criavam suas próprias funções para tratar datas, o que podia levar à inconsistência e bugs. Hoje, usar bibliotecas como `moment.js` ou `date-fns` é comum porque fornecem APIs mais robustas e manipulação precisa de datas. Estas bibliotecas lidam com detalhes como formatos de datas internacionais, fusos horários e horário de verão de forma automática. O `moment.js` já foi o padrão de fato, enquanto o `date-fns` oferece funcionalidades similares com uma abordagem modular.

Alternativas nativas como `Intl.DateTimeFormat` também estão emergindo com suporte à internacionalização, mas podem ser limitadas se comparadas às bibliotecas especializadas.

## See Also
- [date-fns Documentation](https://date-fns.org/docs/Getting-Started)
- [moment.js Documentation](https://momentjs.com/docs/)
- [MDN Web Docs - Date](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [MDN Web Docs - Intl.DateTimeFormat](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Intl/DateTimeFormat)
