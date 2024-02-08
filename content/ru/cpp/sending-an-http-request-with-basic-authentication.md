---
title:                "Отправка HTTP-запроса с базовой аутентификацией"
aliases:
- ru/cpp/sending-an-http-request-with-basic-authentication.md
date:                  2024-01-29T00:02:40.105672-07:00
model:                 gpt-4-0125-preview
simple_title:         "Отправка HTTP-запроса с базовой аутентификацией"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/cpp/sending-an-http-request-with-basic-authentication.md"
changelog:
  - 2024-01-29, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и Почему?

Отправка HTTP-запроса с базовой аутентификацией включает в себя прикрепление имени пользователя и пароля к запросу для контроля доступа. Программисты делают это для простых схем аутентификации, чтобы защитить ресурсы на сервере.

## Как это сделать:

Вот базовый пример использования библиотеки `CURL` на C++. Прежде чем начать, убедитесь, что у вас установлен `libcurl`.

```C++
#include <iostream>
#include <curl/curl.h>

// Простая функция обратного вызова для обработки данных, полученных с помощью curl
static size_t WriteCallback(void *contents, size_t size, size_t nmemb, void *userp) {
    ((std::string*)userp)->append((char*)contents, size * nmemb);
    return size * nmemb;
}

int main() {
    CURL *curl;
    CURLcode res;
    std::string readBuffer;

    curl = curl_easy_init();
    if(curl) {
        curl_easy_setopt(curl, CURLOPT_URL, "http://yourapi.com/data");
        curl_easy_setopt(curl, CURLOPT_HTTPAUTH, CURLAUTH_BASIC);
        curl_easy_setopt(curl, CURLOPT_USERNAME, "user");
        curl_easy_setopt(curl, CURLOPT_PASSWORD, "pass");
        curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, WriteCallback);
        curl_easy_setopt(curl, CURLOPT_WRITEDATA, &readBuffer);
        
        // Выполнение запроса и проверка на наличие ошибок
        res = curl_easy_perform(curl);
        if(res != CURLE_OK) {
            fprintf(stderr, "curl_easy_perform() failed: %s\n", curl_easy_strerror(res));
        } else {
            std::cout << readBuffer << std::endl;
        }
        
        // Очистка
        curl_easy_cleanup(curl);
    }
    return 0;
}
```

Вы увидите ответ от сервера, выведенный в консоль, если не возникло ошибок.

## Глубокое погружение

Базовая аутентификация — это старая методика, которая появилась в начале существования HTTP. Сейчас в индустрии предпочтение отдаётся более безопасным методам, таким как OAuth и токены. Несмотря на это, базовая аутентификация до сих пор используется, часто для внутренних или простых систем, где тяжёлые слои безопасности являются чрезмерным перебором.

Под капотом ваше имя пользователя и пароль кодируются в base64 и помещаются в HTTP-заголовок. Это просто, но не безопасно, если не использовать HTTPS, поскольку base64 легко обратим — использование HTTPS обязательно.

Если `libcurl` вам не по душе, рассмотрите альтернативы, такие как библиотека `cpp-httplib`, или можете воспользоваться `Boost.Beast` для более практичного подхода.

## См. также

- [libcurl](https://curl.se/libcurl/)
- [репозиторий cpp-httplib на GitHub](https://github.com/yhirose/cpp-httplib)
- [документация Boost.Beast](https://www.boost.org/doc/libs/master/libs/beast/doc/html/index.html)
