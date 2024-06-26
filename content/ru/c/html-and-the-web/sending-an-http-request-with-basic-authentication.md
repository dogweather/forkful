---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:09:31.769125-07:00
description: "\u041A\u0430\u043A \u044D\u0442\u043E \u0441\u0434\u0435\u043B\u0430\
  \u0442\u044C: \u0427\u0442\u043E\u0431\u044B \u043E\u0442\u043F\u0440\u0430\u0432\
  \u0438\u0442\u044C HTTP-\u0437\u0430\u043F\u0440\u043E\u0441 \u0441 \u0431\u0430\
  \u0437\u043E\u0432\u043E\u0439 \u0430\u0443\u0442\u0435\u043D\u0442\u0438\u0444\u0438\
  \u043A\u0430\u0446\u0438\u0435\u0439 \u043D\u0430 C, \u043D\u0430\u043C \u043F\u043E\
  \u043D\u0430\u0434\u043E\u0431\u0438\u0442\u0441\u044F \u0438\u0441\u043F\u043E\u043B\
  \u044C\u0437\u043E\u0432\u0430\u0442\u044C \u0431\u0438\u0431\u043B\u0438\u043E\u0442\
  \u0435\u043A\u0443 libcurl, \u043F\u043E\u043F\u0443\u043B\u044F\u0440\u043D\u0443\
  \u044E,\u2026"
lastmod: '2024-03-13T22:44:45.916381-06:00'
model: gpt-4-0125-preview
summary: "\u0427\u0442\u043E\u0431\u044B \u043E\u0442\u043F\u0440\u0430\u0432\u0438\
  \u0442\u044C HTTP-\u0437\u0430\u043F\u0440\u043E\u0441 \u0441 \u0431\u0430\u0437\
  \u043E\u0432\u043E\u0439 \u0430\u0443\u0442\u0435\u043D\u0442\u0438\u0444\u0438\u043A\
  \u0430\u0446\u0438\u0435\u0439 \u043D\u0430 C, \u043D\u0430\u043C \u043F\u043E\u043D\
  \u0430\u0434\u043E\u0431\u0438\u0442\u0441\u044F \u0438\u0441\u043F\u043E\u043B\u044C\
  \u0437\u043E\u0432\u0430\u0442\u044C \u0431\u0438\u0431\u043B\u0438\u043E\u0442\u0435\
  \u043A\u0443 libcurl, \u043F\u043E\u043F\u0443\u043B\u044F\u0440\u043D\u0443\u044E\
  , \u043C\u043D\u043E\u0433\u043E\u0444\u0443\u043D\u043A\u0446\u0438\u043E\u043D\
  \u0430\u043B\u044C\u043D\u0443\u044E \u0438 \u043F\u0440\u043E\u0441\u0442\u0443\
  \u044E \u0432 \u0438\u0441\u043F\u043E\u043B\u044C\u0437\u043E\u0432\u0430\u043D\
  \u0438\u0438 \u043A\u043B\u0438\u0435\u043D\u0442\u0441\u043A\u0443\u044E \u0431\
  \u0438\u0431\u043B\u0438\u043E\u0442\u0435\u043A\u0443 \u0434\u043B\u044F \u043F\
  \u0435\u0440\u0435\u0434\u0430\u0447\u0438 URL."
title: "\u041E\u0442\u043F\u0440\u0430\u0432\u043A\u0430 HTTP-\u0437\u0430\u043F\u0440\
  \u043E\u0441\u0430 \u0441 \u0431\u0430\u0437\u043E\u0432\u043E\u0439 \u0430\u0443\
  \u0442\u0435\u043D\u0442\u0438\u0444\u0438\u043A\u0430\u0446\u0438\u0435\u0439"
weight: 45
---

## Как это сделать:
Чтобы отправить HTTP-запрос с базовой аутентификацией на C, нам понадобится использовать библиотеку libcurl, популярную, многофункциональную и простую в использовании клиентскую библиотеку для передачи URL. Она обрабатывает различные протоколы, включая HTTP и HTTPS, что упрощает нашу задачу. Убедитесь, что libcurl установлена в вашей системе перед началом работы. Вот базовый пример, демонстрирующий, как отправить GET-запрос с базовой аутентификацией:

```c
#include <stdio.h>
#include <curl/curl.h>

int main(void) {
    CURL *curl;
    CURLcode res;

    curl_global_init(CURL_GLOBAL_DEFAULT);

    curl = curl_easy_init();
    if(curl) {
        // URL, на который отправляется запрос
        curl_easy_setopt(curl, CURLOPT_URL, "http://example.com/resource");
        // Включение использования базовой аутентификации
        curl_easy_setopt(curl, CURLOPT_HTTPAUTH, CURLAUTH_BASIC);
        // Предоставление имени пользователя и пароля для базовой аутентификации
        curl_easy_setopt(curl, CURLOPT_USERPWD, "username:password");

        // Выполнение GET-запроса
        res = curl_easy_perform(curl);

        // Проверка на ошибки
        if(res != CURLE_OK)
            fprintf(stderr, "curl_easy_perform() failed: %s\n",
                    curl_easy_strerror(res));

        // Всегда выполняем очистку
        curl_easy_cleanup(curl);
    }
    
    curl_global_cleanup();

    return 0;
}
```
В приведенном выше примере замените `"http://example.com/resource"`, `"username"` и `"password"` на ваш фактический URL, имя пользователя и пароль.

Этот код инициализирует объект `CURL`, устанавливает URL, включает HTTP-базовую аутентификацию и указывает учетные данные. Затем он отправляет запрос и выполняет за собой очистку. Если запрос выполнен успешно, запрашиваемый ресурс будет получен; если возникла ошибка, она будет напечатана в stderr.

Пример вывода (при условии успешной аутентификации и доступа к ресурсу) может не отображаться программой напрямую, так как пример в основном демонстрирует отправку запроса. Для вывода ответа программу можно расширить для обработки данных HTTP-ответа.

## Подробный анализ:
Отправка HTTP-запросов с базовой аутентификацией на C, как показано, использует библиотеку libcurl из-за ее надежности и простоты. Исторически создание HTTP-запросов исключительно на C без таких библиотек было громоздким и подверженным ошибкам процессом, который включал программирование на уровне сокетов и ручное создание HTTP-заголовков.

Сама базовая аутентификация - это метод из первых дней веба. Она отправляет учетные данные в легко декодируемом формате (Base64), что является не безопасным по открытым каналам. Современные приложения часто предпочитают более безопасные методы аутентификации, такие как OAuth 2.0 или JWT (JSON Web Tokens), особенно для чувствительных данных.

Тем не менее, для внутренних, менее критичных систем или быстрых и грязных скриптов, где удобство перевешивает опасения по поводу безопасности, базовая аутентификация по-прежнему используется. Более того, в сочетании с зашифрованными подключениями (HTTPS) ее простота становится преимуществом для быстрой разработки, тестирования или автоматизации, где более высокие механизмы безопасности не так необходимы.

В контекстах, где передовая безопасность непреложна, следует отдать предпочтение альтернативам, таким как аутентификация на основе токенов. Тем не менее, понимание того, как реализовать базовую аутентификацию на C с помощью libcurl, обеспечивает фундаментальный навык, который можно адаптировать к различным методам аутентификации и протоколам, отражая тонкие компромиссы между безопасностью, удобством и требованиями приложений в веб-разработке.
