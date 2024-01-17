---
title:                "Enviando uma solicitação http com autenticação básica"
html_title:           "Python: Enviando uma solicitação http com autenticação básica"
simple_title:         "Enviando uma solicitação http com autenticação básica"
programming_language: "Python"
category:             "Python"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/python/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## O que & Por quê?

Enviar uma solicitação HTTP com autenticação básica é quando um programa envia uma solicitação a um servidor da web usando um nome de usuário e senha para verificar a identidade do usuário. Os programadores fazem isso para garantir que as informações confidenciais permaneçam restritas apenas a usuários autorizados.

## Como fazer:

```python
import requests

url = "https://exemplo.com/login"
username = "usuario"
password = "senha"

response = requests.get(url, auth=(username, password))

print(response.text)
```

Este exemplo usa o módulo `requests` para enviar uma solicitação GET para a URL especificada, com um nome de usuário e senha fornecidos para a autenticação básica. A resposta da solicitação é então impressa no terminal.

## Mergulho Profundo:

A autenticação básica é um método de autenticação simples que foi introduzido pela primeira vez no protocolo HTTP em 1995. Ele foi projetado para ser facilmente implementado por servidores e clientes e, portanto, usa apenas informações de nome de usuário e senha como credenciais.

Uma alternativa à autenticação básica é a autenticação de desafio-resposta, que é mais segura, pois o servidor cria um desafio para o cliente provar sua identidade. No entanto, a autenticação básica ainda é comumente usada em muitos sistemas e aplicativos.

Ao implementar autenticação básica em seu programa, é importante garantir que as informações de autenticação sejam codificadas em arquivo ou enviadas por HTTPS para evitar que sejam interceptadas.

## Veja também:

- Documentação oficial do módulo `requests`: https://docs.python-requests.org/
- Tutorial sobre autenticação básica com `requests`: https://www.digitalocean.com/community/tutorials/how-to-use-basic-authentication-with-http-with-python-3