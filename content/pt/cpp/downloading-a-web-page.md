---
title:                "C++: Baixando uma página da web"
simple_title:         "Baixando uma página da web"
programming_language: "C++"
category:             "C++"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/cpp/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Por que

Baixar uma página da web pode ser uma tarefa útil para acessar informações específicas ou para fins de análise de dados. Além disso, entender como fazer isso pode ajudar a melhorar suas habilidades de programação em C++.

## Como fazer

Aqui está um exemplo de como baixar uma página da web usando C++:

```C++
#include <iostream>
#include <curl/curl.h>

using namespace std;

// Esta função será usada para armazenar o conteúdo da página da web em uma string
size_t write_callback(char *ptr, size_t size, size_t nmemb, string *data)
{
    data->append(ptr, size * nmemb);
    return size * nmemb;
}

int main()
{
    CURL *curl;
    CURLcode res;
    // URL da página que queremos baixar
    string url = "https://www.example.com/";

    // Inicializa a biblioteca curl
    curl = curl_easy_init();
    // Verifica se houve algum erro na inicialização
    if (curl)
    {
        // Define a URL a ser baixada
        curl_easy_setopt(curl, CURLOPT_URL, url.c_str());
        // Define a função de callback para armazenar o conteúdo
        curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, write_callback);
        // Passa o endereço da string onde o conteúdo será armazenado
        string data;
        curl_easy_setopt(curl, CURLOPT_WRITEDATA, &data);
        // Executa a ação de download
        res = curl_easy_perform(curl);
        // Verifica se houve algum erro
        if (res != CURLE_OK)
            cerr << "Erro ao baixar a página: " << curl_easy_strerror(res) << endl;
        else
        {
            // Imprime o conteúdo armazenado na string
            cout << data << endl;
        }
        // Encerra a biblioteca curl
        curl_easy_cleanup(curl);
    }
    return 0;
}
```

A saída do código acima irá imprimir todo o conteúdo da página da web especificada. Você também pode modificar o código para realizar outras ações, como salvar o conteúdo em um arquivo.

## Profundidade Técnica

Para baixar uma página da web em C++, utilizamos a biblioteca libcurl. Primeiro, inicializamos a biblioteca com a função `curl_easy_init()` e, em seguida, podemos definir as opções de download usando a função `curl_easy_setopt()`. A opção `CURLOPT_URL` especifica a URL que queremos baixar, a opção `CURLOPT_WRITEFUNCTION` define a função de callback que será usada para armazenar o conteúdo e a opção `CURLOPT_WRITEDATA` passa o endereço da string onde o conteúdo será armazenado. Por fim, executamos a ação de download com `curl_easy_perform()` e encerramos a biblioteca com `curl_easy_cleanup()`.

## Veja também

- [Documentação da biblioteca libcurl](https://curl.se/libcurl/)
- [Exemplos de uso da biblioteca libcurl em C++](https://curl.se/libcurl/cplusplus/) 
- [Tutorial sobre como utilizar a biblioteca libcurl](https://codingislove.com/download-url-content-curl/)