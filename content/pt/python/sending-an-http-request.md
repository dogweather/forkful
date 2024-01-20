---
title:                "Enviando uma solicitação http"
html_title:           "Bash: Enviando uma solicitação http"
simple_title:         "Enviando uma solicitação http"
programming_language: "Python"
category:             "Python"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/python/sending-an-http-request.md"
---

{{< edit_this_page >}}

Título: Programação Python: Como Enviar um Pedido HTTP

## O que e por que?

Um pedido HTTP (Hypertext Transfer Protocol request) é uma forma padrão de se comunicar pela internet. Programadores em Python enviam pedidos HTTP para interagir com a web - seja para consultar uma API, enviar dados, ou até mesmo para rastrear páginas da web.

## Como fazer:

Com Python, você pode usar a biblioteca `requests` para fazer um pedido HTTP. Veja como fazer um pedido GET simples:
```Python
import requests

resposta = requests.get('https://www.google.com')
print(resposta.status_code)
```
Aqui, você está pedindo a página inicial do Google e imprimindo o status do código HTTP da resposta.

## Mergulho Profundo

O protocolo HTTP foi desenvolvido por Tim Berners-Lee, o inventor da web, em 1989. Com o Python, além da biblioteca `requests`, também podemos usar `httplib2`, `treq` e `aiohttp` para enviar solicitações HTTP.

A biblioteca `requests` é a escolha principal de muitos desenvolvedores devido à sua simplicidade. No entanto, `httplib2` também é uma ótima alternativa, pois permite que você reutilize o mesmo objeto de conexão para várias solicitações (o que pode ser útil se você estiver fazendo muitas solicitações para o mesmo servidor). O `aiohttp` é excelente para pedidos assíncronos.

Aqui estão alguns detalhes de implementação para enviar um pedido HTTP considerando o método POST:

```Python
import requests

dados = {'chave': 'valor'}
resposta = requests.post('https://www.exemplo.com', data=dados)
print(resposta.status_code)
print(resposta.text)
```

Neste exemplo, `data=dados` envia os dados no formato 'form-encoded'. Se quiser enviar como um JSON, basta substituir `data=dados` por `json=dados`.

```Python
import requests

dados = {'chave': 'valor'}
resposta = requests.post('https://www.exemplo.com', json=dados)
print(resposta.status_code)
print(resposta.text)
```

## Veja Também

* Documentação oficial do [`httplib2`](https://httplib2.readthedocs.io/en/latest/)
* Documentação oficial do [`aiohttp`](https://docs.aiohttp.org/en/stable/)
* [Guia HTTP para iniciantes da Mozilla](https://developer.mozilla.org/pt-BR/docs/Web/HTTP/Overview), excelente recurso para entender os fundamentos do HTTP.