---
title:                "Enviando uma requisição HTTP com autenticação básica"
date:                  2024-01-20T18:02:18.871370-07:00
model:                 gpt-4-1106-preview
simple_title:         "Enviando uma requisição HTTP com autenticação básica"
programming_language: "Python"
category:             "Python"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/python/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## O Que & Por Quê?
Enviar uma requisição HTTP com autenticação básica é o processo de acessar recursos protegidos em um servidor web, usando um nome de usuário e senha codificados em base64. Programadores fazem isso para interagir com APIs que exigem um nível de segurança, garantindo que só usuários autorizados tenham acesso.

## Como Fazer:
```Python
import requests
from requests.auth import HTTPBasicAuth

url = "http://algumsite.com/api/recurso"
username = "seu_usuario"
password = "sua_senha"

# Enviar a requisição com autenticação básica
response = requests.get(url, auth=HTTPBasicAuth(username, password))

print(response.status_code)
print(response.json())

# Muitas vezes você pode simplificar usando apenas (username, password):
response_simples = requests.get(url, auth=(username, password))

print(response_simples.status_code)
print(response_simples.json())
```
Saída esperada:
```
200
{"dados": "valor dos dados"}
200
{"dados": "valor dos dados"}
```

## Aprofundando:
A autenticação básica HTTP é um método antigo, mas ainda em uso devido à sua simplicidade. No passado, era a forma principal de autenticar, mas agora muitos consideram pouco segura porque as credenciais podem ser facilmente interceptadas se não usadas com HTTPS. Alternativas incluem autenticação Digest, OAuth e tokens de acesso.

A codificação Base64 não é uma tática de criptografia, o que significa que, embora obscureça suas credenciais, não as protege de olhares curiosos. Sempre use HTTPS quando enviar informações sensíveis.

Implementar corretamente a autenticação pode ser a diferença entre um sistema seguro e um convite aberto a vulnerabilidades. Ao desenvolver aplicações que se comunicam com APIs que necessitam de autenticação, garanta que você está seguindo as melhores práticas de segurança e dando atenção especial ao gerenciamento das credenciais.

## Veja Também:
- Documentação oficial do Requests sobre autenticação: https://docs.python-requests.org/en/latest/user/authentication/#basic-authentication
- RFC 7617, 'The 'Basic' HTTP Authentication Scheme': https://tools.ietf.org/html/rfc7617
- Mais sobre segurança com autenticação básica HTTP: https://www.owasp.org/index.php/Basic_Authentication