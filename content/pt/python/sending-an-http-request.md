---
title:                "Python: Enviando uma solicitação http"
simple_title:         "Enviando uma solicitação http"
programming_language: "Python"
category:             "Python"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/python/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Por que enviar uma solicitação HTTP é importante?

Enviar uma solicitação HTTP pode ser útil para acessar dados de uma página da web ou interagir com um servidor. É uma maneira eficaz de se comunicar com recursos externos e obter informações relevantes para o seu código.

## Como enviar uma solicitação HTTP em Python

Para enviar uma solicitação HTTP em Python, você pode usar a biblioteca "requests". Antes de enviar a solicitação, certifique-se de importar a biblioteca em seu código.

```
# Importe a biblioteca requests
import requests

# Faça uma solicitação GET para uma página da web
resposta = requests.get("https://exemplo.com")

# Imprima o status code da resposta
print(resposta.status_code)

# Imprima o conteúdo da resposta
print(resposta.text)
```
A saída do código acima seria algo parecido com isso:
```
200
<html>
    <head>
        <title>Exemplo</title>
    </head>
    <body>
        <h1>Bem-vindo ao exemplo.com!</h1>
    </body>
</html>
```
Neste exemplo, enviamos uma solicitação GET para a página "exemplo.com" e imprimimos o status code da resposta (200, que significa que a solicitação foi bem-sucedida) e o conteúdo da resposta (a página HTML).

## Uma visão mais aprofundada sobre o envio de solicitações HTTP

Enquanto o exemplo acima mostra como enviar uma solicitação HTTP básica, existem muitos outros parâmetros que podem ser utilizados para personalizar sua solicitação. Por exemplo, você pode adicionar cabeçalhos, parâmetros, autenticação e muito mais à sua solicitação HTTP.

Além disso, é importante entender os diferentes métodos de solicitação HTTP, como GET, POST, PUT e DELETE, e quando usá-los corretamente. Isso pode afetar o resultado da sua solicitação e a interação com o servidor.

No entanto, antes de enviar qualquer solicitação HTTP, é essencial ter uma boa compreensão dos padrões e regras de cada servidor e ter cuidado para não enviar muitas solicitações em um curto período de tempo, evitando sobrecarregar o servidor.

## Veja também

- [Documentação do requests](https://requests.readthedocs.io/en/latest/)
- [W3Schools - HTTP Requests](https://www.w3schools.com/python/module_requests.asp)
- [Tutoriais Point - HTTP Requests](https://www.tutorialspoint.com/python_network_programming/python_http.htm)