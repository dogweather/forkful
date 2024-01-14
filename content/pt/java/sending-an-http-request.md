---
title:                "Java: Enviando uma solicitação http"
simple_title:         "Enviando uma solicitação http"
programming_language: "Java"
category:             "Java"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/java/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Por que enviar uma solicitação HTTP?

Enviar solicitações HTTP é uma tarefa comum na programação Java, especialmente quando se trabalha com APIs e serviços na web. Essas solicitações permitem que os desenvolvedores interajam com outros sistemas, obtendo informações ou realizando ações específicas. É uma parte essencial do desenvolvimento de aplicativos e pode ser útil de várias maneiras.

## Como fazer

Enviar uma solicitação HTTP em Java é relativamente simples. Você precisará de uma biblioteca externa, como o Apache HttpClient, para facilitar a interação com o servidor. Aqui está um exemplo de como enviar uma solicitação GET e obter a resposta em formato JSON:

```Java
// Importar as bibliotecas necessárias
import org.apache.http.HttpResponse;
import org.apache.http.client.methods.HttpGet;
import org.apache.http.impl.client.CloseableHttpClient;
import org.apache.http.impl.client.HttpClientBuilder;
import org.json.JSONObject;

// Criar um objeto HttpClient
CloseableHttpClient httpClient = HttpClientBuilder.create().build();

// Crie um objeto HttpGet com o URL da solicitação
HttpGet httpGet = new HttpGet("https://api.example.com/users");

// Executar a solicitação e obter a resposta
HttpResponse response = httpClient.execute(httpGet);

// Extrair o conteúdo da resposta como um objeto JSON
JSONObject jsonResponse = new JSONObject(response.getEntity().getContent());

// Imprimir a resposta
System.out.println(jsonResponse.toString());
```

A saída deste código será um objeto JSON contendo informações sobre os usuários da API. Você pode personalizar sua solicitação, adicionando parâmetros ou headers, dependendo das necessidades do seu projeto.

## Aprofundando

Embora o exemplo acima mostre como enviar uma solicitação GET simples, há muito mais a considerar ao trabalhar com solicitações HTTP. Algumas coisas a se manter em mente incluem autenticação, tratamento de erros e uso adequado de métodos HTTP, como PUT e POST.

Para mais informações, você pode consultar a documentação da biblioteca que está utilizando ou ler sobre como funcionam as solicitações HTTP em geral. É importante entender completamente esse processo para garantir que suas solicitações sejam eficazes e eficientes.

## Veja também

- [Documentação do Apache HttpClient](https://hc.apache.org/httpcomponents-client-ga/index.html)
- [Tutorial de solicitação HTTP em Java](https://www.baeldung.com/httpclient-guide)
- [Visão geral de solicitações HTTP](https://developer.mozilla.org/pt-BR/docs/Web/HTTP/Overview)