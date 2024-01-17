---
title:                "Enviando uma solicitação http"
html_title:           "Java: Enviando uma solicitação http"
simple_title:         "Enviando uma solicitação http"
programming_language: "Java"
category:             "Java"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/java/sending-an-http-request.md"
---

{{< edit_this_page >}}

## O que & Porquê?

Em termos simples, enviar uma solicitação HTTP é um processo em que um cliente envia uma solicitação a um servidor com o objetivo de obter informações ou realizar uma ação específica. Os programadores utilizam isso para interagir com APIs, acessar dados de um servidor remoto e realizar operações da web.

## Como fazer:

```Java
//Exemplo de código para enviar uma solicitação GET usando a biblioteca Apache HttpClient
HttpGet req = new HttpGet("https://exemplo.com/recurso1?id=1");
HttpClient client = HttpClient.newHttpClient();
HttpResponse res = client.execute(req);
int status = res.getStatusLine().getStatusCode();
System.out.println("Status code: " + status);

//Exemplo de código para enviar uma solicitação POST usando a biblioteca HttpURLConnection
URL url = new URL("https://exemplo.com/api/usuario");
HttpURLConnection con = (HttpURLConnection) url.openConnection();
con.setRequestMethod("POST");
con.setDoOutput(true);
DataOutputStream out = new DataOutputStream(con.getOutputStream());
String body = "nome=João&idade=25";
out.writeBytes(body);
out.flush();
out.close();
int status = con.getResponseCode();
System.out.println("Status code: " + status);
con.disconnect();
```

Saída para ambos os exemplos:

```
Status code: 200
```

## Mergulho Profundo:

As solicitações HTTP foram introduzidas pela primeira vez em 1994 como uma forma de solicitar páginas da web em um navegador. Hoje, elas são utilizadas em uma ampla variedade de aplicações e serviços, como aplicações mobile, IoT e comunicação entre servidores.

Além das bibliotecas mencionadas nos exemplos, existem outras alternativas para enviar solicitações HTTP em Java, como a biblioteca OkHttp e as APIs Java para HTTP/2 e HTTP/3.

Quando se trata de implementação, é importante estar ciente de questões de segurança, como autenticação e criptografia de dados sensíveis em uma solicitação. Também é recomendável familiarizar-se com diferentes métodos HTTP, como GET, POST, PUT e DELETE, e quando utilizá-los adequadamente.

## Veja também:

- [Documentação oficial do Java para HttpURLConnection](https://docs.oracle.com/en/java/javase/11/docs/api/java.net/java/net/HttpURLConnection.html)
- [Tutorial da Apache HTTP Client](https://hc.apache.org/httpcomponents-client-ga/tutorial/html/index.html)
- [Documentação oficial do Java para HTTP/2 e HTTP/3](https://docs.oracle.com/en/java/javase/11/docs/api/java.net.http/java/net/http/HttpClient.html)
- [Site oficial do OkHttp](https://square.github.io/okhttp/)