---
title:                "Enviando uma requisição http"
html_title:           "Javascript: Enviando uma requisição http"
simple_title:         "Enviando uma requisição http"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/javascript/sending-an-http-request.md"
---

{{< edit_this_page >}}

##Por que enviar uma solicitação HTTP?

Enviar uma solicitação HTTP é uma forma comum de interagir com servidores e obter informações deles. Isso permite que os sites se comuniquem com outras APIs, obtenham dados de um banco de dados ou executem outras operações no servidor.

##Como fazer:

Para enviar uma solicitação HTTP em Javascript, usamos o objeto XMLHttpRequest. Aqui está um exemplo de como podemos enviar uma solicitação GET para uma API e obter sua resposta:

```Javascript
const request = new XMLHTTPRequest();
request.open('GET', 'https://api.exemplo.com/users');
request.send();

request.onload = () => {
    console.log(request.response);
}
```

No código acima, criamos uma nova instância do objeto XMLHttpRequest e abrimos uma solicitação GET para a URL fornecida. Em seguida, enviamos a solicitação e, quando a resposta estiver pronta, o método onload é acionado. Podemos então acessar a resposta usando o atributo 'response' do objeto.

##Aprofundando:

Além de enviar solicitações simples, podemos especificar o tipo de solicitação (GET, POST, PUT, DELETE), adicionar cabeçalhos personalizados e enviar dados no corpo da solicitação. Também podemos lidar com erros usando os métodos onerror ou ontimeout do objeto. Para saber mais sobre o objeto XMLHttpRequest e todas as suas propriedades e métodos, você pode consultar a documentação da MDN.

##Veja também:

- [Documentação MDN do objeto XMLHttpRequest](https://developer.mozilla.org/pt-BR/docs/Web/API/XMLHttpRequest)
- [Artigo sobre métodos HTTP](https://www.treinaweb.com.br/blog/o-que-sao-os-metodos-http-e-para-que-servem/)
- [Tutorial sobre como enviar solicitações HTTP em Javascript](https://www.digitalocean.com/community/tutorials/como-usar-a-api-web-http-com-javascript)