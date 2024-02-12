---
title:                "Baixando uma página da web"
aliases:
- /pt/c/downloading-a-web-page.md
date:                  2024-02-03T17:55:49.745769-07:00
model:                 gpt-4-0125-preview
simple_title:         "Baixando uma página da web"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/c/downloading-a-web-page.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## O Que & Por Quê?

Baixar uma página da web em C envolve acessar programaticamente o conteúdo de uma página da web pela internet e salvá-lo localmente para processamento ou uso offline. Programadores frequentemente se engajam nisso para consumir serviços da web, raspar conteúdo da web ou interagir diretamente com recursos online a partir de suas aplicações.

## Como fazer:

Para baixar uma página da web em C, uma abordagem popular é usar a biblioteca libcurl, uma biblioteca de transferência de URL do lado do cliente eficiente e portátil. Certifique-se de ter o libcurl instalado e vinculado ao seu projeto. Aqui está um exemplo demonstrando como usar o libcurl para baixar o conteúdo de uma página da web:

```c
#include <stdio.h>
#include <curl/curl.h>

size_t write_data(void *ptr, size_t size, size_t nmemb, FILE *stream) {
    size_t written = fwrite(ptr, size, nmemb, stream);
    return written;
}

int main(void) {
    CURL *curl;
    FILE *fp;
    CURLcode res;
    char *url = "http://example.com";
    char outfilename[FILENAME_MAX] = "./downloaded_page.html";

    curl = curl_easy_init(); // Inicializa uma sessão fácil libcurl
    if (curl) {
        fp = fopen(outfilename,"wb");
        curl_easy_setopt(curl, CURLOPT_URL, url);
        curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, write_data); // Callback para escrever os dados recebidos
        curl_easy_setopt(curl, CURLOPT_WRITEDATA, fp); // Define o ponteiro do arquivo para escrever os dados

        res = curl_easy_perform(curl); // Realiza o download do arquivo
        if(res != CURLE_OK) {
            fprintf(stderr, "curl_easy_perform() falhou: %s\n",
                    curl_easy_strerror(res));
        }

        /* sempre limpe */
        curl_easy_cleanup(curl); // Limpa a sessão fácil
        fclose(fp); // Fecha o fluxo de arquivo
    }
    return 0;
}
```
Saída de exemplo (nenhuma saída visível no console): Esse código baixa o conteúdo no URL especificado e o salva em um arquivo chamado `downloaded_page.html`. Verifique o diretório do seu programa para este arquivo para ver o conteúdo baixado.

## Aprofundamento:

Historicamente, o download de conteúdo da web em C era mais complicado, exigindo programação de soquete manual e manipulação do protocolo HTTP. Libcurl abstrai essas complexidades, oferecendo uma API de alto nível robusta para transferência de dados pela web.

Embora o libcurl simplifique as requisições HTTP em C, linguagens de programação modernas como Python, com sua biblioteca `requests`, ou JavaScript (Node.js) com várias bibliotecas de clientes HTTP, podem oferecer uma sintaxe mais intuitiva e suporte embutido para JSON e outros formatos de dados comumente usados na comunicação web. No entanto, C e libcurl fornecem uma solução de alta performance e estável para sistemas onde a eficiência, controle detalhado ou integração em bases de código C existentes são críticos. Também vale ressaltar que C, combinado com libcurl, pode ser usado para mais do que apenas baixar páginas da web - ele é capaz de lidar com FTP, SMTP e muito mais, tornando-o uma ferramenta versátil no conjunto de ferramentas de um programador.
