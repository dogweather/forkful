---
title:                "Enviando uma solicitação http"
html_title:           "Python: Enviando uma solicitação http"
simple_title:         "Enviando uma solicitação http"
programming_language: "Python"
category:             "Python"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/python/sending-an-http-request.md"
---

{{< edit_this_page >}}

## O que e por que?

Enviar uma solicitação HTTP é um processo comum na programação web, em que um programa de computador faz uma solicitação a um servidor para obter informações ou executar uma ação. Programadores geralmente fazem isso para obter dados de fontes externas ou interagir com outras aplicações utilizando APIs.

## Como fazer:

Para enviar uma solicitação HTTP em Python, o pacote "requests" pode ser utilizado com apenas algumas linhas de código. Veja um exemplo abaixo:

```Python
import requests

# Fazendo uma solicitação GET
response = requests.get("https://exemplo.com")
print(response.status_code) # Imprime o código de status da resposta
print(response.text) # Imprime o conteúdo da resposta
```

Saída:
```
200
<html><body>Exemplo de conteúdo</body></html>
```

## Aprofundando:

Enviar uma solicitação HTTP é algo que é feito desde o início da internet. Originalmente, os protocolos utilizados eram o HTTP e o FTP, mas com o crescimento dos serviços web, o HTTP se tornou mais popular. Existem outras formas de fazer solicitações HTTP em Python, como utilizando a biblioteca "urllib", mas o pacote "requests" oferece uma interface mais simples e amigável. Além disso, é possível customizar a solicitação com parâmetros opcionais, como headers e cookies.

## Veja também:

- [Documentação do pacote "requests"](https://requests.readthedocs.io/en/latest/)
- [Exemplos de uso do pacote "requests"](https://realpython.com/python-requests/)
- [Comparação entre os pacotes "requests" e "urllib"](https://www.geeksforgeeks.org/python-difference-between-windowsurllib-request-and-requests-httprequest/)