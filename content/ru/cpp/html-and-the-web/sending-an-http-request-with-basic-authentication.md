---
changelog:
- 2024-01-29, gpt-4-0125-preview, translated from English
date: 2024-01-29 00:02:40.105672-07:00
description: "\u041E\u0442\u043F\u0440\u0430\u0432\u043A\u0430 HTTP-\u0437\u0430\u043F\
  \u0440\u043E\u0441\u0430 \u0441 \u0431\u0430\u0437\u043E\u0432\u043E\u0439 \u0430\
  \u0443\u0442\u0435\u043D\u0442\u0438\u0444\u0438\u043A\u0430\u0446\u0438\u0435\u0439\
  \ \u0432\u043A\u043B\u044E\u0447\u0430\u0435\u0442 \u0432 \u0441\u0435\u0431\u044F\
  \ \u043F\u0440\u0438\u043A\u0440\u0435\u043F\u043B\u0435\u043D\u0438\u0435 \u0438\
  \u043C\u0435\u043D\u0438 \u043F\u043E\u043B\u044C\u0437\u043E\u0432\u0430\u0442\u0435\
  \u043B\u044F \u0438 \u043F\u0430\u0440\u043E\u043B\u044F \u043A \u0437\u0430\u043F\
  \u0440\u043E\u0441\u0443 \u0434\u043B\u044F \u043A\u043E\u043D\u0442\u0440\u043E\
  \u043B\u044F \u0434\u043E\u0441\u0442\u0443\u043F\u0430. \u041F\u0440\u043E\u0433\
  \u0440\u0430\u043C\u043C\u0438\u0441\u0442\u044B\u2026"
lastmod: '2024-02-25T18:49:43.266047-07:00'
model: gpt-4-0125-preview
summary: "\u041E\u0442\u043F\u0440\u0430\u0432\u043A\u0430 HTTP-\u0437\u0430\u043F\
  \u0440\u043E\u0441\u0430 \u0441 \u0431\u0430\u0437\u043E\u0432\u043E\u0439 \u0430\
  \u0443\u0442\u0435\u043D\u0442\u0438\u0444\u0438\u043A\u0430\u0446\u0438\u0435\u0439\
  \ \u0432\u043A\u043B\u044E\u0447\u0430\u0435\u0442 \u0432 \u0441\u0435\u0431\u044F\
  \ \u043F\u0440\u0438\u043A\u0440\u0435\u043F\u043B\u0435\u043D\u0438\u0435 \u0438\
  \u043C\u0435\u043D\u0438 \u043F\u043E\u043B\u044C\u0437\u043E\u0432\u0430\u0442\u0435\
  \u043B\u044F \u0438 \u043F\u0430\u0440\u043E\u043B\u044F \u043A \u0437\u0430\u043F\
  \u0440\u043E\u0441\u0443 \u0434\u043B\u044F \u043A\u043E\u043D\u0442\u0440\u043E\
  \u043B\u044F \u0434\u043E\u0441\u0442\u0443\u043F\u0430. \u041F\u0440\u043E\u0433\
  \u0440\u0430\u043C\u043C\u0438\u0441\u0442\u044B\u2026"
title: "\u041E\u0442\u043F\u0440\u0430\u0432\u043A\u0430 HTTP-\u0437\u0430\u043F\u0440\
  \u043E\u0441\u0430 \u0441 \u0431\u0430\u0437\u043E\u0432\u043E\u0439 \u0430\u0443\
  \u0442\u0435\u043D\u0442\u0438\u0444\u0438\u043A\u0430\u0446\u0438\u0435\u0439"
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
