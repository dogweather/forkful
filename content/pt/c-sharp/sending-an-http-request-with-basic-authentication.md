---
title:                "Enviando uma solicitação http com autenticação básica"
html_title:           "Clojure: Enviando uma solicitação http com autenticação básica"
simple_title:         "Enviando uma solicitação http com autenticação básica"
programming_language: "C#"
category:             "C#"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/c-sharp/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

# Enviar uma solicitação HTTP com autenticação básica em C#

## O Que & Por Quê?

Enviar uma solicitação HTTP com autenticação básica é, basicamente, pedir ao servidor um determinado recurso fornecendo um nome de usuário e senha para provar sua identidade. Fazemos isso para acessar recursos protegidos em um servidor, que requerem algum nível de autorização para serem acessados. 

## Como Fazer:

Em C#, usamos a classe HttpClient para enviar uma solicitação HTTP. Para adicionar a autenticação básica, adicionamos um header de autenticação no request.

```C#
using System;
using System.Net.Http;
using System.Text;

class Programa 
{
    static async Task Main()
    {
        var cliente = new HttpClient();
        
        var byteArray = Encoding.ASCII.GetBytes("usuario:senha");
        cliente.DefaultRequestHeaders.Authorization = new 
        System.Net.Http.Headers.AuthenticationHeaderValue("Basic", Convert.ToBase64String(byteArray));
        
        HttpResponseMessage resposta = await cliente.GetAsync("http://url-do-seu-servidor");
        
        Console.WriteLine(resposta.StatusCode);
    }
}
```

Este exemplo é bem direto. Ele primeiro adiciona "Basic [base64 de 'usuario:senha']" ao cabeçalho da autenticação, então fazemos uma solicitação GET para a URL do servidor. A resposta será o status da solicitação.

## Olhando Mais a Fundo:

**Contexto Histórico**: A autenticação básica é um esquema de autenticação antigo, uma das primeiras maneiras de controlar o acesso a páginas web. Hoje em dia, é menos comum, devido à sua fragilidade. A senha codificada em base64 pode ser facilmente decodificada, comprometendo a segurança.

**Alternativas**: Uma substituição mais segura para a autenticação básica seria usar autenticação de token ou OAuth. Nestes esquemas, o cliente recebe um token em vez de fornecer um nome de usuário e senha para cada solicitação.

**Detalhes de Implementação**: Quando você coloca as credenciais no cabeçalho de autorização, elas são apenas codificadas em base64, não criptografadas. Isso significa que alguém com acesso aos pacotes HTTP podem simplesmente decodificar a string base64 e obter as credenciais. Portanto, é altamente recomendável usar HTTPS quando se trabalha com autenticação básica.

## Veja Também:

- [HttpClient Class (Microsoft Documentation)](https://docs.microsoft.com/en-us/dotnet/api/system.net.http.httpclient)
- [Basic Access Authentication (Wikipedia)](https://en.wikipedia.org/wiki/Basic_access_authentication)
- [OAuth (Wikipedia)](https://en.wikipedia.org/wiki/OAuth)
- [The Dangers of Basic Authentication](https://securityboulevard.com/2020/02/the-dangers-of-basic-authentication/)