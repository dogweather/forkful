---
title:                "Python: Enviando uma solicitação http com autenticação básica"
simple_title:         "Enviando uma solicitação http com autenticação básica"
programming_language: "Python"
category:             "Python"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/python/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Por que enviar uma solicitação HTTP com autenticação básica?

Existem várias razões pelas quais alguém pode querer enviar uma solicitação HTTP com autenticação básica. Por exemplo, pode ser uma forma segura de se comunicar com um servidor remoto para obter ou enviar dados confidenciais. Além disso, a autenticação básica é um dos métodos mais simples e amplamente suportados de autenticação em sistemas web.

## Como fazer

Para enviar uma solicitação HTTP com autenticação básica em Python, primeiro precisamos importar o módulo `requests`:

```Python
import requests
```

Em seguida, definimos as credenciais de autenticação em uma variável `credenciais` no formato `username:password`. É importante notar que essas informações devem ser codificadas em Base64 antes de serem enviadas na solicitação.

```Python
credenciais = b"usuario:senha"
```

Então, criamos um cabeçalho de autenticação contendo a palavra "Basic" seguida pelas credenciais codificadas:

```Python
cabecalho = {"Authorization": "Basic " + b64encode(credenciais).decode('utf-8')}
```

Por fim, basta fazer uma solicitação HTTP para o URL desejado e passar o cabeçalho contendo as credenciais:

```Python
resposta = requests.get("https://exemplo.com", headers=cabecalho)
```

Se a autenticação for bem-sucedida, a resposta conterá os dados que foram solicitados.

## Aprofundando-se

O método de autenticação básica funciona enviando as credenciais em formato de texto simples na solicitação HTTP. No entanto, isso significa que se um invasor interceptar a solicitação, ele terá acesso às informações de login.

Uma maneira de aumentar a segurança é utilizar o protocolo HTTPS, que criptografa os dados durante a transmissão. Além disso, a autenticação básica pode ser combinada com outros métodos de autenticação, como o OAuth, para adicionar uma camada extra de proteção.

## Veja também

- [Documentação oficial do módulo `requests`](https://requests.readthedocs.io/)
- [Artigo sobre segurança em comunicações HTTP/HTTPS](https://blog.sympt.ai/2019/02/07/basic-auth-in-http-vs-https-authentication/)