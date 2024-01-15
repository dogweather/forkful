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

## Por que

Você já se deparou com uma página da web que solicitava um nome de usuário e senha antes de permitir o acesso? Isso é conhecido como autenticação básica e, neste artigo, vamos explorar por que e como enviar uma solicitação HTTP com autenticação básica usando Python.

## Como Fazer

Primeiro, precisamos importar o módulo `requests` em nosso código Python:

```Python
import requests
```

Agora, podemos definir as informações de autenticação básica, incluindo o nome de usuário e senha, e adicioná-las ao cabeçalho da nossa solicitação:

```Python
username = "seu_nome_de_usuario"
password = "sua_senha"

headers = {"Authorization": f"Basic {username}:{password}"}
```

Em seguida, podemos enviar a solicitação HTTP para o URL desejado, incluindo o cabeçalho que acabamos de criar:

```Python
url = "https://www.exemplo.com"
response = requests.get(url, headers=headers)
```

Por fim, podemos imprimir o código de status da resposta para verificar se a solicitação foi bem-sucedida:

```Python
print(response.status_code)
```

Se tudo correr como planejado, você deve receber um código de status `200`, indicando que a solicitação foi bem-sucedida.

## Deep Dive

A autenticação básica é um método simples de autenticação, mas não é tão seguro quanto outros métodos mais avançados. Quando enviamos uma solicitação HTTP com autenticação básica, o nome de usuário e senha são codificados em Base64, mas não são criptografados. Isso significa que, se alguém interceptar a solicitação, pode decifrar facilmente as informações de autenticação e acessar a sua conta. Portanto, é importante sempre usar a autenticação básica em conjunto com HTTPS (HTTP seguro) para garantir a segurança das suas informações.

## Veja Também

- [Documentação oficial do módulo requests](https://docs.python-requests.org/en/master/)
- [Tutorial de autenticação básica utilizando Python](https://www.freecodecamp.org/news/basic-authentication-in-python/)
- [Explicação detalhada sobre autenticação básica](https://www.restapitutorial.com/httpstatuscodes.html#basicauthentication)