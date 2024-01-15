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

## Por que
Você já se perguntou como os aplicativos e sites que usamos todos os dias se comunicam com os servidores? Isso é feito através do envio de solicitações HTTP. Neste artigo, vamos aprender como enviar uma solicitação HTTP usando Java e por que isso é importante para os desenvolvedores.

## Como Fazer
Para enviar uma solicitação HTTP em Java, nós precisamos seguir alguns passos simples. Primeiro, vamos criar uma instância da classe `URLConnection` e fornecer a URL que queremos enviar a solicitação. Em seguida, vamos configurar os parâmetros da solicitação, como o método (GET, POST, PUT, etc.), cabeçalhos e corpo da requisição. Por fim, executamos a solicitação e obtemos a resposta do servidor.

```java
//Criando uma instância de URLConnection
URL url = new URL("http://www.exemplo.com");
URLConnection con = url.openConnection();

//Configurando a solicitação
con.setDoOutput(true); //nós queremos enviar dados
con.setRequestMethod("POST"); //método POST
con.setRequestProperty("Content-Type", "application/json"); //definindo o tipo de conteúdo do corpo

//Escrevendo o corpo da solicitação
OutputStream os = con.getOutputStream();
byte[] input = "{'mensagem': 'Olá servidor!'}".getBytes("utf-8");
os.write(input, 0, input.length);

//Executando a solicitação
InputStream in = con.getInputStream();

//Lendo a resposta do servidor
int responseCode = ((HttpURLConnection)con).getResponseCode();
System.out.println("Código de resposta: " + responseCode);
```

O código acima mostra como enviar uma solicitação HTTP POST com um corpo JSON e ver a resposta do servidor. Você pode usar esse mesmo processo para enviar solicitações com outros métodos e tipos de conteúdo.

## Mergulho Profundo
Para enviar uma solicitação HTTP em Java, também é possível utilizar as bibliotecas Apache HttpClient e HttpURLConnection. Ambas fornecem uma API mais avançada para lidar com solicitações HTTP, como autenticação, cookies e redirecionamentos.

Além disso, é importante entender os diferentes códigos de resposta HTTP que podemos receber do servidor. Alguns exemplos comuns incluem `200` para solicitação bem-sucedida, `400` para solicitação inválida e `500` para erro do servidor.

## Veja também
- [Documentação oficial do Java sobre HttpURLConnection](https://docs.oracle.com/javase/7/docs/api/java/net/HttpURLConnection.html)
- [Tutorial da Apache HttpClient](https://hc.apache.org/httpcomponents-client-ga/tutorial/html/fundamentals.html)
- [Lista de códigos de resposta HTTP](https://developer.mozilla.org/pt-BR/docs/Web/HTTP/Status)