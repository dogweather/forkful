---
title:                "Convertendo uma data em uma string"
html_title:           "Javascript: Convertendo uma data em uma string"
simple_title:         "Convertendo uma data em uma string"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/javascript/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

O que é e porquê?

Converter uma data em uma string é um processo comum em programação, onde uma data no formato numérico é convertida para uma representação em texto. Isso é útil para mostrar datas de forma legível para os usuários ou para operações de manipulação de dados em strings.

Como fazer:

```javascript
// Exemplo 1:
let data = new Date(); // Cria um objeto de data com a data e hora atuais
let dataString = data.toDateString(); // Converte a data para uma string no formato "Fri Jun 18 2021"

// Exemplo 2:
let data2 = new Date("2021-06-18"); // Cria um objeto de data com a data especificada
let dataString2 = data2.toLocaleDateString("pt-BR"); // Converte a data para uma string no formato "18/06/2021", seguindo o formato local do Brasil
```

Resultados:

```
Exemplo 1:
Fri Jun 18 2021

Exemplo 2:
18/06/2021
```

Detalhes:

Converter uma data para uma string é um processo importante em programação, pois permite que os programadores apresentem informações de data e hora de forma legível para os usuários. Além disso, ao converter uma data em uma string, é possível realizar operações de manipulação de dados em strings, como concatenação e comparação.

Uma alternativa ao método `toString()` utilizado nos exemplos acima é o método `toLocaleDateString()`, que permite especificar o formato e idioma da string resultante.

Os métodos `toString()` e `toLocaleDateString()` não modificam o objeto de data original, apenas retornam a representação da data em forma de string.

Veja também:

- [Documentação oficial do método `toString()`](https://developer.mozilla.org/pt-BR/docs/Web/JavaScript/Reference/Global_Objects/Date/toString)
- [Documentação oficial do método `toLocaleDateString()`](https://developer.mozilla.org/pt-BR/docs/Web/JavaScript/Reference/Global_Objects/Date/toLocaleDateString)