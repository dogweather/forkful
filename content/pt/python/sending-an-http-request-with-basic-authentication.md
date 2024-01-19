---
title:                "Enviando uma solicitação http com autenticação básica"
html_title:           "Clojure: Enviando uma solicitação http com autenticação básica"
simple_title:         "Enviando uma solicitação http com autenticação básica"
programming_language: "Python"
category:             "Python"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/python/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## O Que & Por Que?

Enviar um pedido HTTP com autenticação básica é uma prática comum na programação da web, que consiste em passar informações de login do usuário (geralmente um nome de usuário e senha) no cabeçalho HTTP de um pedido. Os programadores fazem isso para obter acesso a serviços e recursos restritos ou protegidos.

## Como Fazer:

Vamos usar a biblioteca `requests` do Python para demonstrar isso. Se você não a tiver instalado, use o comando pip `pip install requests`.

```Python
import requests
from requests.auth import HTTPBasicAuth

resposta = requests.get('https://seusiteprotegido.com', auth=HTTPBasicAuth('usuario', 'senha'))

print(resposta.status_code)
```
O script acima deverá retornar o código de status HTTP da resposta. 200 significa sucesso!

## Deep Dive

A autenticação básica HTTP foi uma das primeiras formas de autenticação implementadas na web. Embora simples e fácil de implementar, ela transmite as credenciais em texto simples (base64 codificado, na verdade, o que é praticamente o mesmo), o que não é seguro sem uma conexão HTTPS.

Existem alternativas mais seguras, como a autenticação Digest ou a OAuth. Contudo, para algumas aplicações internas simples ou para fins de teste, a autenticação básica pode ser adequada.

Quando você usa a função `HTTPBasicAuth()` no seu pedido, a biblioteca `requests` adiciona automaticamente o cabeçalho `Authorization` ao seu pedido HTTP, preenchido com as palavras 'Basic' seguidas pelas suas credenciais codificadas em base64.

## Veja Também

- Documentação oficial da biblioteca Python `requests`: https://docs.python-requests.org/en/latest/
- Tutorial mais detalhado sobre autenticação com `requests`: https://realpython.com/python-requests/#authentication
- W3C na autenticação HTTP básica: https://tools.ietf.org/html/rfc7617