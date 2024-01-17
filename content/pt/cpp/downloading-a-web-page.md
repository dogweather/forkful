---
title:                "Baixando uma página da web."
html_title:           "C++: Baixando uma página da web."
simple_title:         "Baixando uma página da web."
programming_language: "C++"
category:             "C++"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/cpp/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## O que é e por que fazer isso?
Fazer o download de uma página da web é quando um programa ou script baixa todo o código HTML, imagens e outros arquivos associados em uma página da web. Programadores geralmente fazem isso para automatizar o processo de obtenção de informações de uma página da web ou para criar ferramentas para análise de dados.

## Como fazer:
Para fazer o download de uma página da web em C++, você pode utilizar a biblioteca libcurl. Veja abaixo um exemplo de código simples que faz o download da página principal do Google e imprime o conteúdo em um arquivo texto:

```C++
#include <iostream>
#include <fstream>
#include <curl/curl.h>

using namespace std;

// Função callback que será chamada para escrever os dados recebidos em um arquivo texto
static size_t WriteCallback(void *contents, size_t size, size_t nmemb, void *userp) {
    ((string*)userp)->append((char*)contents, size * nmemb);
    return size * nmemb;
}

int main() {
    // Iniciando sessão do libcurl
    CURL *curl;
    CURLcode res;
    curl = curl_easy_init();
    
    if(curl) {
        // Configurando a URL a ser baixada
        curl_easy_setopt(curl, CURLOPT_URL, "https://www.google.com/");

        // Definindo função de callback para escrever os dados recebidos
        curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, WriteCallback);
        
        // Variável para armazenar os dados recebidos
        string content;
        
        // Configurando ponteiro para variável que irá armazenar os dados recebidos
        curl_easy_setopt(curl, CURLOPT_WRITEDATA, &content);

        // Realizando o download
        res = curl_easy_perform(curl);
        
        // Verificando por possíveis erros
        if(res != CURLE_OK) {
            cout << "Erro no download: " << curl_easy_strerror(res) << endl;
        }
        else {
            // Imprimindo os dados recebidos em um arquivo texto
            ofstream output_file;
            output_file.open("google_page.txt");
            output_file << content;
            output_file.close();
        }

        // Limpando a sessão do libcurl
        curl_easy_cleanup(curl);
    }
    
    return 0;
}
```

## Deep Dive:
Fazer o download de páginas da web é uma tarefa bem comum em programação e muitas vezes é necessário para a automação de processos ou análise de dados. Além da biblioteca libcurl, existem outras opções para fazer o download de páginas da web em C++, como a biblioteca cURLpp, que oferece interfaces de nível mais alto.

Para implementar um downloader de páginas da web é necessário ter um bom conhecimento de HTTP e de como as requisições e respostas são feitas. Também é importante ter cuidado com possíveis erros durante a realização do download e tratar adequadamente os dados recebidos.

## Veja também:
- [Documentação da biblioteca libcurl](https://curl.se/libcurl/c/)
- [Documentação da biblioteca cURLpp](https://www.manpagez.com/html/curlpp/curlpp-0.7.3/curlpp.30.php)