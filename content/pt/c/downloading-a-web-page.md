---
title:                "Baixando uma página da web"
date:                  2024-01-20T17:43:24.912664-07:00
model:                 gpt-4-1106-preview
simple_title:         "Baixando uma página da web"
programming_language: "C"
category:             "C"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/c/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## O Que & Porquê?
Baixar uma página da web significa pegar o conteúdo dela - geralmente HTML - através da Internet. Programadores fazem isso para processar informações, verificar disponibilidade ou interagir com serviços web.

## Como Fazer:
Vamos usar a biblioteca `libcurl` no C para baixar o conteúdo de uma página. Primeiro, instale a biblioteca, se ainda não tiver feito:

```sh
sudo apt-get install libcurl4-openssl-dev
```

Agora, o código:

```c
#include <stdio.h>
#include <curl/curl.h>

size_t write_data(void *ptr, size_t size, size_t nmemb, void *stream) {
    return fwrite(ptr, size, nmemb, (FILE *)stream);
}

int main(void) {
    CURL *curl;
    FILE *fp;
    CURLcode res;
    char *url = "http://example.com";
    char outfilename[FILENAME_MAX] = "downloaded_page.html";
    
    curl = curl_easy_init();
    if (curl) {
        fp = fopen(outfilename,"wb");
        curl_easy_setopt(curl, CURLOPT_URL, url);
        curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, write_data);
        curl_easy_setopt(curl, CURLOPT_WRITEDATA, fp);
        res = curl_easy_perform(curl);
        curl_easy_cleanup(curl);
        fclose(fp);
    }
    return 0;
}
```

Compile com:

```sh
gcc -o webpage_downloader main.c -lcurl
```

E execute:

```sh
./webpage_downloader
```

Isto baixará o conteúdo de `http://example.com` e o salvará no arquivo `downloaded_page.html`.

## Mergulho Profundo:
Baixar páginas da web é um recurso usado desde o início da Internet. Protocolos como HTTP e FTP existem há décadas. Antes do `libcurl`, ferramentas como o `wget` e o `curl` (linha de comando) eram frequentemente usadas em scripts bash.

Alternativas incluem outras bibliotecas como `libwww` ou `WinINet` no Windows. Em termos de implementação, estabelece-se uma conexão TCP/IP com o servidor, envia-se um pedido HTTP GET e processa-se a resposta.

Finalmente, `libcurl` oferece mais do que só baixar - também pode enviar dados (POST, PUT), trabalhar com autenticação, cookies e muito mais. É uma biblioteca estável, segura e versátil.

## Veja Também:
- Documentação do libcurl: https://curl.se/libcurl/c/
- Tutorial de libcurl para C: https://curl.se/libcurl/c/libcurl-tutorial.html
- Artigo sobre HTTP: https://developer.mozilla.org/pt-BR/docs/Web/HTTP
- Introdução ao protocolo FTP: https://www.rfc-editor.org/rfc/rfc959