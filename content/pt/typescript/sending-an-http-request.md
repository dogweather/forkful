---
title:                "Enviando uma solicitação http"
html_title:           "TypeScript: Enviando uma solicitação http"
simple_title:         "Enviando uma solicitação http"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/typescript/sending-an-http-request.md"
---

{{< edit_this_page >}}

O que e Por que?

Enviar uma solicitação HTTP é um processo essencial em programação, pois permite que o seu código faça solicitações a um servidor ou outro recurso online. Isso é especialmente importante quando se trabalha com aplicativos da web ou APIs.

Como fazer:

```TypeScript
import axios from 'axios';

axios.get('https://api.example.com/users')
  .then((response) => {
    console.log(response.data);
  })
  .catch((error) => {
    console.log(error);
  });
```

Saída de amostra:

```
[{name: 'João', age: 25}, {name: 'Maria', age: 30}, {name: 'Carlos', age: 22}]
```

Mergulho profundo:

Na era moderna da web, enviar solicitações HTTP é uma parte importante da construção de aplicativos. Antes do advento do AJAX (Asynchronous JavaScript and XML), a página inteira precisava ser recarregada para cada solicitação. O AJAX permitiu que o código do lado do cliente fizesse solicitações dinâmicas ao servidor, tornando os aplicativos da web mais rápidos e eficientes. Existem várias bibliotecas e frameworks de JavaScript que facilitam o envio de solicitações HTTP, como o popular Axios.

Veja também:

- O guia oficial do TypeScript sobre o uso de bibliotecas de terceiros: [https://www.typescriptlang.org/docs/handbook/declaration-files/library-structures.html](https://www.typescriptlang.org/docs/handbook/declaration-files/library-structures.html)
- Uma introdução ao conceito de AJAX: [https://developer.mozilla.org/en-US/docs/Web/Guide/AJAX](https://developer.mozilla.org/en-US/docs/Web/Guide/AJAX)
- Documentação do Axios: [https://github.com/axios/axios](https://github.com/axios/axios)