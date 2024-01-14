---
title:                "Javascript: Enviando uma solicitação http"
simple_title:         "Enviando uma solicitação http"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/javascript/sending-an-http-request.md"
---

{{< edit_this_page >}}

# Por que enviar uma solicitação HTTP?

Enviar uma solicitação HTTP é uma parte essencial da programação em JavaScript. Isso permite que os desenvolvedores obtenham informações de outros servidores, integrem APIs e criem aplicativos mais dinâmicos e interativos.

# Como enviar uma solicitação HTTP

Para enviar uma solicitação HTTP em JavaScript, primeiro é necessário criar um objeto XMLHttpRequest. Isso pode ser feito da seguinte forma:

```Javascript
var request = new XMLHttpRequest();
```

Em seguida, é preciso definir o método da solicitação, o URL de destino e se a solicitação deve ser assíncrona ou não:

```Javascript
request.open('GET', 'https://www.exemplo.com', true);
```

Para enviar a solicitação, é necessário chamar o método `send()` e, em seguida, tratar a resposta com o método `onreadystatechange`:

```Javascript
request.onreadystatechange = function() {
  if (this.readyState == 4 && this.status == 200) {
    console.log(this.responseText);
  }
};
request.send();
```

O código acima enviará uma solicitação GET para https://www.exemplo.com e, se for bem-sucedida, irá imprimir a resposta no console.

# Aprofundando-se no envio de solicitações HTTP

Existem outras propriedades que podem ser definidas ao enviar uma solicitação HTTP, como os cabeçalhos (`setRequestHeader()`) e o corpo da solicitação (`send()`). Além disso, é possível enviar solicitações POST, PUT, DELETE e outras, dependendo da sua aplicação. É importante entender os diferentes códigos de status HTTP que podem ser retornados e como tratá-los adequadamente.

# Veja também

- [Usando XMLHttpRequest](https://developer.mozilla.org/pt-BR/docs/Web/API/XMLHttpRequest/Usando_XMLHttpRequest)
- [Introdução às solicitações http em JavaScript](https://www.digitalocean.com/community/tutorials/js-ajax-http-request)
- [HTTP request methods](https://developer.mozilla.org/pt-BR/docs/Web/HTTP/Methods)