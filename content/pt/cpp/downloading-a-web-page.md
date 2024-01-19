---
title:                "Baixando uma página da web"
html_title:           "Bash: Baixando uma página da web"
simple_title:         "Baixando uma página da web"
programming_language: "C++"
category:             "C++"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/cpp/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## O que & Por quê?

Baixar uma página da web é o processo de copiar os dados de um site para o seu computador local. Programadores fazem isso para analisar ou manipular esses dados para várias finalidades, como rastreamento da web ou testes automatizados.

## Como fazer:

Neste exemplo, usaremos a biblioteca cURL para baixar uma página da web em C++. A instalação do cURL pode variar dependendo do seu sistema operacional.

```C++
#include <iostream>
#include <string>
#include <curl/curl.h>

std::size_t callback(
    const char* in,
    std::size_t size,
    std::size_t num,
    std::string* out)
{
    const std::size_t totalBytes(size * num);
    out->append(in, totalBytes);
    return totalBytes;
}
        
int main() {
    CURL* curl = curl_easy_init();
    // Define URL to download
    curl_easy_setopt(curl, CURLOPT_URL, "http://example.com");

    // Define callback to handle downloaded data
    std::string response;
    curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, callback);
    curl_easy_setopt(curl, CURLOPT_WRITEDATA, &response);
    
    // Perform the request
    curl_easy_perform(curl);
    
    // Print the downloaded data
    std::cout << response;
    
    curl_easy_cleanup(curl);

    return 0;
}
```

Este código irá baixar a página web de "http://example.com" e imprimir o conteúdo HTML no console.

## Mergulho Profundo

Registrar páginas da web é uma prática comum na programação desde que a Internet se tornou popular. Existem várias maneiras de fazer isso em C++, e a biblioteca cURL é apenas uma delas.

Alternativamente, você também pode fazer uso do `Boost.Asio` para tarefas de rede mais complexas ou a `cpp-httplib` para casos de uso mais simples.

Ao usar bibliotecas como o cURL, é importante considerar o gerenciamento de memória e gestão dos dados baixados. No exemplo fornecido, os dados são apenas anexados a uma string. No entanto, se os dados fossem enormes, você precisaria pensar em estratégias para processá-los eficientemente.

## Veja Também

Alguns recursos úteis são a documentação oficial do cURL (https://curl.se/libcurl/c/) e a Boost.Asio (https://www.boost.org/doc/libs/1_66_0/doc/html/boost_asio.html). A biblioteca cpp-httplib também tem um repositório GitHub (https://github.com/yhirose/cpp-httplib) com exemplos e informações úteis.