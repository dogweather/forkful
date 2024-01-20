---
title:                "Baixando uma página da web"
html_title:           "Bash: Baixando uma página da web"
simple_title:         "Baixando uma página da web"
programming_language: "C"
category:             "C"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/c/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## O Que & Por Que?

Baixar uma página da web é a tarefa de copiar seu conteúdo - geralmente o HTML, CSS, e JavaScript - do servidor para o computador local. Programadores fazem isso para manipular, vasculhar, ou reexibir o conteúdo da página de forma programática.  

## Como Fazer:

Vamos usar a biblioteca curl para fazer isso em C. Aqui está um exemplo simples de como baixar uma página da web:

```C
#include <curl/curl.h>
#include <stdio.h>

size_t write_data(void* buffer, size_t size, size_t nmemb, void* userp)
{
    return fwrite(buffer, size, nmemb, (FILE*)userp);
}

int main(void)
{
    CURL* curl;
    FILE* fp;
    CURLcode res;
    char* url = "http://example.com";
    char outfilename[FILENAME_MAX] = "/tmp/example.html";
    curl_global_init(CURL_GLOBAL_DEFAULT);
    curl = curl_easy_init();
    
    if(curl) {
        fp = fopen(outfilename,"wb");
        
        curl_easy_setopt(curl, CURLOPT_URL, url);
        curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, write_data);
        curl_easy_setopt(curl, CURLOPT_WRITEDATA, fp);
        
        res = curl_easy_perform(curl);
        
        curl_easy_cleanup(curl);
        fclose(fp);
    }
    
    curl_global_cleanup();
    
    return 0;
}
```

Isto vai baixar a página "http://example.com" e guardar o conteúdo num arquivo chamado "/tmp/example.html".

## Mergulho Profundo:

Embora esteja fora de uso hoje em dia, o protocolo HTTP foi historicamente vital para transferir dados através da web, e é isso que metaforicamente 'baixa' uma página web.

Baixar e analisar páginas da web desta maneira é a base do "web scraping", uma técnica usada para obter informações de sites que não oferecem métodos de acesso mais convenientes.

O comando `curl_easy_perform` é onde a mágica acontece. Ele desencadeia a cadeia de chamadas que fará a requisição HTTP e processará o download dos dados, chamando a função de retorno `write_data` para gravar os dados em um arquivo conforme eles chegam.

## Ver Também:

* Documentação oficial de libcurl: https://curl.haxx.se/libcurl/c/
* Httpbin para testar requisições HTTP: https://httpbin.org
* Web scraping com Beautiful Soup (em Python): https://www.crummy.com/software/BeautifulSoup/