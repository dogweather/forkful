---
title:                "Baixando uma página da web"
html_title:           "Java: Baixando uma página da web"
simple_title:         "Baixando uma página da web"
programming_language: "Java"
category:             "Java"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/java/downloading-a-web-page.md"
---

{{< edit_this_page >}}

##Por que
Baixar uma página da web é útil para coletar informações de um site específico, como dados de preços, notícias ou qualquer conteúdo que possa ser útil para análise ou uso pessoal.

##Como Fazer
Para baixar uma página da web em Java, podemos usar a classe `URL` e a classe `URLConnection`. Primeiro, precisamos criar uma instância de `URL` passando o endereço do site como parâmetro. Em seguida, podemos abrir uma conexão e ler os dados da página usando um `BufferedReader`.

```Java
URL url = new URL("https://www.example.com");
URLConnection con = url.openConnection();
BufferedReader reader = new BufferedReader(new InputStreamReader(con.getInputStream()));
String line;
while ((line = reader.readLine()) != null) {
    System.out.println(line);
}
```

O código acima irá imprimir o conteúdo da página linha por linha. Também podemos usar um `FileWriter` para salvar o conteúdo em um arquivo local.

##Deep Dive
Quando fazemos uma solicitação para uma URL, o site pode retornar uma resposta com o código de status HTTP e os cabeçalhos. Podemos acessar essas informações usando os métodos `getResponseCode()` e `getHeaderField()`. Além disso, também podemos especificar um tempo limite para a conexão e adicionar cabeçalhos personalizados.

Veja mais informações sobre a classe `URL` e a classe `URLConnection` na [documentação oficial do Java](https://docs.oracle.com/javase/tutorial/networking/urls/readingWriting.html).

##Veja Também
- [Como fazer solicitações HTTP em Java](https://www.baeldung.com/java-http-request) 
- [Como analisar dados HTML em Java](https://www.baeldung.com/java-html-parsing)