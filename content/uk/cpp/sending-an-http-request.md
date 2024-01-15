---
title:                "Надсилання http-запиту"
html_title:           "C++: Надсилання http-запиту"
simple_title:         "Надсилання http-запиту"
programming_language: "C++"
category:             "C++"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/cpp/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Чому
Якщо ви займаєтеся розробкою веб-сайтів або додатків, то, ймовірно, ви зіткнулися з необхідністю відправляти HTTP-запити. Це є важливою частиною створення зв'язку між клієнтом та сервером, тому що за допомогою HTTP-запиту можна отримувати та відправляти дані.

## Як
```C++
#include <iostream>
#include <sstream>
#include <curl/curl.h>

int main() {
    CURL *curl;
    CURLcode res;
    std::string url = "https://www.example.com"; // замініть на свій URL
    curl = curl_easy_init();
    if (curl) {
        // Налаштування запиту
        curl_easy_setopt(curl, CURLOPT_URL, url.c_str());
        // Виконання запиту
        res = curl_easy_perform(curl);
        // Перевірка на помилки
        if (res != CURLE_OK)
            std::cerr << "Помилка при виконанні запиту: " << curl_easy_strerror(res) << std::endl;
        // Закриваємо ресурси
        curl_easy_cleanup(curl);
    }
    return 0;
}
```
Щоб відправити HTTP-запит за допомогою C++, потрібно використовувати бібліотеку libcurl. У коді ми створюємо змінну `curl`, щоб ініціалізувати бібліотеку і налаштовуємо запит за допомогою функції `curl_easy_setopt()`. Після цього запит виконується за допомогою функції `curl_easy_perform()`. При виникненні помилок, виводимо їх на екран. Не забудьте додати бібліотеку `curl` до свого проекту.

## Глибокий занурення
Лібcurl надає багато можливостей для налаштування запиту. Наприклад, ви можете встановити метод запиту (`GET`, `POST`, `PUT` тощо), додати заголовки, параметри та тіло запиту. Також можна встановити час очікування відповіді сервера та обробляти різні коди відповіді. Для докладнішої інформації про можливості libcurl, звертайтеся до документації.

## Дивіться також
- [libcurl документація](https://curl.se/libcurl/)
- [Простий приклад відправки HTTP-запиту у Java](https://www.codejava.net/java-se/networking/java-http-request-example) 
- [HTTP-запити від Google Developers](https://developers.google.com/web/fundamentals/primers/http)