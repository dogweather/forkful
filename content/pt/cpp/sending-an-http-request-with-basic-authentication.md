---
title:                "Enviando uma solicitação http com autenticação básica"
html_title:           "C++: Enviando uma solicitação http com autenticação básica"
simple_title:         "Enviando uma solicitação http com autenticação básica"
programming_language: "C++"
category:             "C++"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/cpp/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## O que e por que?

A autenticação básica é um método de segurança para acessar um servidor web através de uma solicitação HTTP. É amplamente utilizado por programadores para garantir a segurança e a privacidade de suas solicitações. Ao enviar uma solicitação HTTP com autenticação básica, o programador pode ter certeza de que apenas usuários autorizados terão acesso ao servidor.

## Como fazer:

Para enviar uma solicitação HTTP com autenticação básica, você precisará de uma URL, um nome de usuário e uma senha. Você também precisará incluir um cabeçalho de autenticação na solicitação. Aqui está um exemplo de código em C ++:

```
#include <iostream>
#include <curl/curl.h>

int main() {
    // Defina a URL, o nome de usuário e a senha
    std::string url = "http://exemplo.com/api";
    std::string username = "usuario";
    std::string password = "senha";

    // Inicialize a biblioteca cURL
    curl_global_init(CURL_GLOBAL_ALL);

    // Crie a solicitação HTTP
    CURL *curl = curl_easy_init();
    if (curl) {
        // Defina a URL
        curl_easy_setopt(curl, CURLOPT_URL, url.c_str());
        
        // Defina o nome de usuário e a senha
        curl_easy_setopt(curl, CURLOPT_USERNAME, username.c_str());
        curl_easy_setopt(curl, CURLOPT_PASSWORD, password.c_str());
        
        // Execute a solicitação
        CURLcode res = curl_easy_perform(curl);
        
        // Verifique o código de resposta
        if (res == CURLE_OK) {
            // Processar a resposta
            std::cout << "Solicitação enviada com sucesso!";
        } else {
            // Processar o erro
            std::cerr << "Erro ao enviar solicitação";
        }
        
        // Limpar
        curl_easy_cleanup(curl);
    } else {
        // Processar o erro
        std::cerr << "Erro ao inicializar cURL";
    }

    // Limpar a biblioteca cURL
    curl_global_cleanup();

    return 0;
}
```

A saída será "Solicitação enviada com sucesso!" se tudo correr bem.

## Mergulho profundo:

A autenticação básica foi criada em 1996 como parte da especificação HTTP/1.0. É considerada um método de autenticação em nível de usuário e é menos segura do que outros métodos como autenticação digest. Uma alternativa para a autenticação básica é o uso de tokens de acesso, que são únicos para cada solicitação e não expõem a senha do usuário.

Ao enviar uma solicitação HTTP com autenticação básica, o cabeçalho de autenticação deve ser codificado usando Base64. Apesar de ser amplamente utilizado, este método não é recomendado para uso em produção, pois é vulnerável a ataques de interceptação de rede e hackers podem facilmente decodificar o cabeçalho.

## Ver também:

- [Código de exemplo usando a biblioteca cURL](https://curl.haxx.se/libcurl/c/simple.html)
- [Alternativas à autenticação básica](https://www.owasp.org/index.php/Authentication_Cheat_Sheet#Authentication_Schemes)
- [Especificação HTTP/1.0](https://tools.ietf.org/html/rfc1945)