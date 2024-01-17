---
title:                "Enviando uma solicitação http com autenticação básica"
html_title:           "C#: Enviando uma solicitação http com autenticação básica"
simple_title:         "Enviando uma solicitação http com autenticação básica"
programming_language: "C#"
category:             "C#"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/c-sharp/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## O que é e por quê?
Enviar uma solicitação HTTP com autenticação básica é uma forma de autenticar as requisições feitas a um servidor. Programadores utilizam esse método para garantir que apenas usuários autorizados tenham acesso aos dados ou recursos disponíveis no servidor.

## Como fazer:
Para enviar uma solicitação HTTP com autenticação básica em C#, siga os seguintes passos:
1. Primeiro, importe os seguintes namespaces:
```C#
using System.Net;
using System.Text;
```
2. Em seguida, configure as credenciais do usuário e a URL do servidor:
```C#
string username = "usuario";
string password = "senha";
string url = "http://exemplo.com";
```
3. Crie a requisição HTTP e adicione as credenciais:
```C#
HttpWebRequest request = (HttpWebRequest)WebRequest.Create(url);
request.Headers.Add("Authorization", "Basic " + Convert.ToBase64String(Encoding.ASCII.GetBytes(username + ":" + password)));
```
4. Realize a solicitação e obtenha a resposta do servidor:
```C#
HttpWebResponse response = (HttpWebResponse)request.GetResponse();
```
5. Finalmente, leia o conteúdo da resposta e imprima na tela:
```C#
Console.WriteLine(new StreamReader(response.GetResponseStream()).ReadToEnd());
```
A saída esperada é o conteúdo da resposta do servidor.

## Aprofundando:
- O método de autenticação básica foi introduzido no protocolo HTTP em 1996, tornando-se amplamente utilizado para autenticar solicitações em aplicações web. No entanto, ele não é considerado uma forma segura de autenticação, pois as credenciais são enviadas em texto legível e podem ser facilmente interceptadas.
- Existem alternativas mais seguras, como a autenticação básica digest, onde as credenciais são criptografadas antes de serem enviadas, e a autenticação OAuth, que é amplamente utilizada em aplicações web modernas.
- Para implementar corretamente a autenticação básica em uma aplicação, é importante seguir as boas práticas de segurança, como armazenar as credenciais de forma segura e utilizar conexões HTTPS para evitar que as informações sejam interceptadas.

## Veja também:
- Documentação da classe HttpWebRequest: https://docs.microsoft.com/pt-br/dotnet/api/system.net.httpwebrequest?view=net-5.0
- Mais informações sobre autenticação básica: https://developer.mozilla.org/pt-BR/docs/Web/HTTP/Authentication