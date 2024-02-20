---
date: 2024-01-20 18:00:28.560747-07:00
description: "Enviar um pedido HTTP \xE9 como voc\xEA faz seu c\xF3digo conversar\
  \ com a web. Programadores fazem isso para buscar dados, enviar informa\xE7\xF5\
  es, interagir com APIs \u2013\u2026"
lastmod: 2024-02-19 22:05:05.220798
model: gpt-4-1106-preview
summary: "Enviar um pedido HTTP \xE9 como voc\xEA faz seu c\xF3digo conversar com\
  \ a web. Programadores fazem isso para buscar dados, enviar informa\xE7\xF5es, interagir\
  \ com APIs \u2013\u2026"
title: "Enviando uma requisi\xE7\xE3o HTTP"
---

{{< edit_this_page >}}

## O Que & Por Que?

Enviar um pedido HTTP é como você faz seu código conversar com a web. Programadores fazem isso para buscar dados, enviar informações, interagir com APIs – basicamente, para alimentar seus aplicativos com recursos da internet.

## Como Fazer:

Vamos usar o `requests`, uma biblioteca em Python que facilita a emissão de pedidos HTTP. Primeiro, você precisa ter a biblioteca instalada:

```bash
pip install requests
```

Agora, um exemplo de como enviar um pedido GET e imprimir a resposta:

```Python
import requests

response = requests.get('https://api.exemplo.com/dados')
if response.status_code == 200:
    print(response.json())  # Supondo que a resposta seja JSON.
else:
    print('Algo deu errado!')
```

E para enviar dados com um pedido POST:

```Python
import requests

dados = {'chave': 'valor'}
response = requests.post('https://api.exemplo.com/enviar', json=dados)

if response.status_code == 200:
    print('Dados enviados com sucesso!')
else:
    print('Falha ao enviar dados.')
```

## Mergulho Profundo:

Antes do `requests`, a galera usava `urllib` e `urllib2` – você pode ainda encontrá-los em código antigo, mas eles eram meio complicados de usar. O `requests` simplificou a vida dos desenvolvedores com uma API "menos é mais".

Quanto às alternativas, o `httpx` é o novo queridinho para pedidos HTTP assíncronos, especialmente útil para aplicativos modernos que precisam lidar com concorrência.

Nos bastidores, quando você manda um pedido HTTP, está basicamente enviando uma mensagem formatada rigorosamente (de acordo com o protocolo HTTP) sobre TCP/IP ao servidor, que então te responde na mesma moeda.

## Veja Também:

- Documentação do `requests`: [https://requests.readthedocs.io/](https://requests.readthedocs.io/)
- `httpx`, um pacote alternativo para pedidos HTTP assíncronos: [https://www.python-httpx.org/](https://www.python-httpx.org/)
- RFC 2616, que especifica o protocolo HTTP/1.1: [https://www.ietf.org/rfc/rfc2616.txt](https://www.ietf.org/rfc/rfc2616.txt)
