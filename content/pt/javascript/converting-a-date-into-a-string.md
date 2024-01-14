---
title:    "Javascript: Convertendo uma data em uma string"
keywords: ["Javascript"]
---

{{< edit_this_page >}}

##Por que
Converter uma data em uma string pode ser útil em muitos programas de Javascript, como aplicações web, jogos e aplicativos móveis. Isso permite que você exiba a data de forma legível para o usuário final e oferece mais flexibilidade na manipulação de dados.

##Como Fazer
Para converter uma data em uma string, podemos usar o método `toString()` do objeto `Date`. Veja um exemplo de código abaixo:

```Javascript
let data = new Date();
let data_string = data.toString();

console.log(data_string); //Saída: Ter Jul 07 2020 15:24:56 GMT-0300 (Horário Padrão de Brasília)
```

Podemos também especificar o formato desejado da string usando os métodos `getMonth()`, `getDate()` e `getFullYear()`, que retornam o mês, dia e ano da data, respectivamente. Veja um exemplo de código abaixo:

```Javascript
let data = new Date();
let mes = data.getMonth() + 1; //Adicionando 1 pois o mês inicia em 0 (Janeiro)
let dia = data.getDate();
let ano = data.getFullYear();

let data_string = `${dia}/${mes}/${ano}`;

console.log(data_string); //Saída: 07/07/2020
```

##Deep Dive
Além do `toString()`, existem outros métodos que podem ser usados para converter uma data em uma string, como o `toLocaleDateString()`, que retorna uma string com a data formatada de acordo com a localização do sistema. E para formatos mais complexos, podemos utilizar bibliotecas externas como o Moment.js.

É importante lembrar que a conversão da data em uma string pode variar dependendo da linguagem de programação e do sistema operacional usado. É sempre recomendado consultar a documentação oficial para garantir a compatibilidade e consistência nos resultados.

##Veja também
- Documentação oficial do objeto `Date` no Javascript: https://developer.mozilla.org/pt-BR/docs/Web/JavaScript/Reference/Global_Objects/Date
- Moment.js: https://momentjs.com/