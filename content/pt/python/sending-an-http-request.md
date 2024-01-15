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

## Por que 
Então você está curioso sobre como enviar uma solicitação HTTP? Talvez você esteja tentando automatizar um processo ou acessar dados de um servidor remoto. Independentemente do motivo, enviar uma solicitação HTTP é uma habilidade valiosa para ter em sua caixa de ferramentas de programação. 

## Como fazer
Para enviar uma solicitação HTTP em Python, você precisará da biblioteca padrão `urllib.request`. Aqui está um exemplo básico de como enviar uma solicitação GET e imprimir a resposta:

```python
import urllib.request
response = urllib.request.urlopen("https://www.google.com")
print(response.read())
```

Saída:
```
b'<!doctype html> <html itemscope="" itemtype="http://schema.org/WebPage" lang="en-US"> ...</html>'
```

Se você quiser personalizar sua solicitação, pode adicionar parâmetros adicionais no método `urlopen`. Por exemplo, para enviar uma solicitação POST, você pode fornecer dados no formato de dicionário e especificar o tipo de conteúdo. 

```python
import urllib.request
import urllib.parse
data = urllib.parse.urlencode({"username":"joao", "password":"1234"}).encode()
req = urllib.request.Request("https://www.exemplo.com/login", data=data, method="POST")
resp = urllib.request.urlopen(req)
print(resp.read())
```

Saída:
```
b'<!doctype html> <html itemscope="" itemtype="http://schema.org/WebPage" lang="en-US"> ...</html>'
```

## Mais detalhes
Para obter mais informações sobre o envio de solicitações HTTP em Python, dê uma olhada nas documentações oficiais do `urllib` e do `urllib.request`. Além disso, você pode explorar outras bibliotecas como `requests` e `http.client` para realizar solicitações HTTP de maneira ainda mais eficiente. Não tenha medo de experimentar e descobrir qual abordagem funciona melhor para o seu projeto.

## Veja também
- Documentação oficial do módulo `urllib`: https://docs.python.org/3/library/urllib.html
- Documentação oficial do módulo `urllib.request`: https://docs.python.org/3/library/urllib.request.html
- Documentação oficial do módulo `requests`: https://docs.python-requests.org/en/latest/
- Documentação oficial do módulo `http.client`: https://docs.python.org/3/library/http.client.html