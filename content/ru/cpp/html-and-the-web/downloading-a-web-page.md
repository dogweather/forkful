---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 23:58:07.522978-07:00
description: "\u0421\u043A\u0430\u0447\u0438\u0432\u0430\u043D\u0438\u0435 \u0432\u0435\
  \u0431-\u0441\u0442\u0440\u0430\u043D\u0438\u0446\u044B \u043E\u0437\u043D\u0430\
  \u0447\u0430\u0435\u0442 \u043F\u043E\u043B\u0443\u0447\u0435\u043D\u0438\u0435\
  \ \u0435\u0451 \u0441\u043E\u0434\u0435\u0440\u0436\u0438\u043C\u043E\u0433\u043E\
  , \u043E\u0431\u044B\u0447\u043D\u043E \u0432 \u0444\u043E\u0440\u043C\u0430\u0442\
  \u0435 HTML, \u0434\u043B\u044F \u043F\u0440\u043E\u0441\u043C\u043E\u0442\u0440\
  \u0430 \u0438\u043B\u0438 \u043E\u0431\u0440\u0430\u0431\u043E\u0442\u043A\u0438\
  \ \u043B\u043E\u043A\u0430\u043B\u044C\u043D\u043E. \u041F\u0440\u043E\u0433\u0440\
  \u0430\u043C\u043C\u0438\u0441\u0442\u044B \u0441\u043A\u0430\u0447\u0438\u0432\u0430\
  \u044E\u0442 \u0432\u0435\u0431-\u2026"
lastmod: '2024-03-11T00:14:19.239885-06:00'
model: gpt-4-0125-preview
summary: "\u0421\u043A\u0430\u0447\u0438\u0432\u0430\u043D\u0438\u0435 \u0432\u0435\
  \u0431-\u0441\u0442\u0440\u0430\u043D\u0438\u0446\u044B \u043E\u0437\u043D\u0430\
  \u0447\u0430\u0435\u0442 \u043F\u043E\u043B\u0443\u0447\u0435\u043D\u0438\u0435\
  \ \u0435\u0451 \u0441\u043E\u0434\u0435\u0440\u0436\u0438\u043C\u043E\u0433\u043E\
  , \u043E\u0431\u044B\u0447\u043D\u043E \u0432 \u0444\u043E\u0440\u043C\u0430\u0442\
  \u0435 HTML, \u0434\u043B\u044F \u043F\u0440\u043E\u0441\u043C\u043E\u0442\u0440\
  \u0430 \u0438\u043B\u0438 \u043E\u0431\u0440\u0430\u0431\u043E\u0442\u043A\u0438\
  \ \u043B\u043E\u043A\u0430\u043B\u044C\u043D\u043E. \u041F\u0440\u043E\u0433\u0440\
  \u0430\u043C\u043C\u0438\u0441\u0442\u044B \u0441\u043A\u0430\u0447\u0438\u0432\u0430\
  \u044E\u0442 \u0432\u0435\u0431-\u2026"
title: "\u0417\u0430\u0433\u0440\u0443\u0437\u043A\u0430 \u0432\u0435\u0431-\u0441\
  \u0442\u0440\u0430\u043D\u0438\u0446\u044B"
---

{{< edit_this_page >}}

## Что и почему?
Скачивание веб-страницы означает получение её содержимого, обычно в формате HTML, для просмотра или обработки локально. Программисты скачивают веб-страницы для извлечения данных, отслеживания изменений или интеграции с веб-сервисами.

## Как это сделать:
В текущей версии C++, вы можете использовать библиотеку `CURL` для скачивания веб-контента. Вот простой пример:

```cpp
#include <curl/curl.h>
#include <iostream>
#include <string>

static size_t writeCallback(void* contents, size_t size, size_t nmemb, void* userp){
    ((std::string*)userp)->append((char*)contents, size * nmemb);
    return size * nmemb;
}

int main() {
    CURL* curl;
    CURLcode res;
    std::string readBuffer;

    curl = curl_easy_init();
    if(curl) {
        curl_easy_setopt(curl, CURLOPT_URL, "http://example.com");
        curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, writeCallback);
        curl_easy_setopt(curl, CURLOPT_WRITEDATA, &readBuffer);
        res = curl_easy_perform(curl);
        curl_easy_cleanup(curl);

        if(res == CURLE_OK) {
            std::cout << readBuffer << std::endl;
        }
        else {
            std::cerr << "Ошибка CURL: " << curl_easy_strerror(res) << std::endl;
        }
    }

    return 0;
}
```

Пример вывода:

```html
<!doctype html>
<html>
<head>
    <title>Пример домена</title>
    ...
</head>
<body>
    <div>
        <h1>Пример домена</h1>
        <p>Этот домен используется для иллюстративных примеров в документах. Вы можете использовать этот домен ...</p>
    </div>
</body>
</html>
```

## Углубляемся
Изначально не существовало стандартного способа скачивания веб-страниц с помощью C++. Программисты использовали платформо-зависимые решения или различные сторонние библиотеки. Теперь `libcurl` - это широко поддерживаемая и универсальная библиотека для передачи данных с URL. Компилируемая и связываемая с вашим кодом на C++, curl является предпочтительным инструментом.

Альтернативы libcurl включают HTTPClientSession от Poco и C++ Rest SDK (известный как Casablanca). В то время как libcurl основан на C и является примерно настолько низкоуровневым, насколько это возможно комфортно для HTTP-запросов, Poco и Casablanca предлагают более идиоматичные интерфейсы C++, которые могут нравиться некоторым пользователям.

Под капотом, когда вы скачиваете веб-страницу, в действие вступает протокол HTTP. На сервер отправляется GET-запрос, и, предполагая, что все проходит хорошо, сервер отвечает содержимым в HTTP-ответе.

## См. также
- [Официальный сайт libcurl](https://curl.se/libcurl/)
- [Репозиторий C++ Rest SDK на GitHub](https://github.com/microsoft/cpprestsdk)
- [Проект Poco](https://pocoproject.org/)
- [HTTP на Википедии](https://en.wikipedia.org/wiki/Hypertext_Transfer_Protocol)
