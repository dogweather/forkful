---
title:                "Baixando uma página da web"
html_title:           "C: Baixando uma página da web"
simple_title:         "Baixando uma página da web"
programming_language: "C"
category:             "C"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/c/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Por que?

Você já se perguntou como um site da internet é carregado em seu navegador? Se você está interessado em aprender como os programas se comunicam com a web, este artigo é para você! Vamos explorar como baixar uma página da web usando a linguagem de programação C.

## Como Fazer

Antes de começarmos, é importante lembrar que este artigo assume um conhecimento básico da linguagem C. Se você é novo nesta linguagem, recomendamos que você consulte alguns tutoriais antes de prosseguir.

Para baixar uma página da web em C, precisamos usar uma biblioteca externa chamada "libcurl". Esta biblioteca fornece funções para fazer solicitações HTTP e manipular o conteúdo da resposta. Então, vamos ver como podemos usar essa biblioteca para baixar uma página da web.

Primeiro, precisamos incluir o cabeçalho da biblioteca em nosso código:

```C
#include <curl/curl.h>
```

Em seguida, declaramos uma variável do tipo `CURL` para armazenar nossa sessão de download:

```C
CURL *curl;
```

Em seguida, precisamos configurar nossa sessão com as opções apropriadas, como o URL que queremos baixar e os parâmetros de conexão:

```C
curl = curl_easy_init();
curl_easy_setopt(curl, CURLOPT_URL, "https://www.example.com");
curl_easy_setopt(curl, CURLOPT_FOLLOWLOCATION, 1L);
```

Por fim, precisamos executar a solicitação e salvar o conteúdo da resposta em um arquivo. Vamos usar a função `curl_easy_perform()` para isso:

```C
FILE *file = fopen("pagina.html","wb");
curl_easy_perform(curl, file);
fclose(file);
```

E pronto! Agora temos o conteúdo da página da web baixada e salva em um arquivo chamado "pagina.html". Você pode alterar o nome do arquivo e o URL de acordo com suas necessidades.

## Mergulho Profundo

Se você estiver interessado em explorar mais opções e funcionalidades da biblioteca libcurl, recomendamos conferir a documentação oficial em https://curl.haxx.se/libcurl/. Também existem muitos tutoriais e exemplos on-line que podem ajudá-lo a entender melhor como usar essa biblioteca em seus projetos.

## Veja Também

Aqui estão alguns links que podem ser úteis para expandir seu conhecimento sobre a linguagem C e a comunicação com a web:

- Documentação oficial do libcurl: https://curl.haxx.se/libcurl/
- Tutorial de C da W3Schools: https://www.w3schools.in/c-tutorial/
- Princípios de comunicação com a web em C: https://dev.to/vaidehijoshi/communications-protocols-should-know-when-coding-in-c-3akp