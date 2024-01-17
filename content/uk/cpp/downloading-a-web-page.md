---
title:                "Завантаження веб-сторінки"
html_title:           "C++: Завантаження веб-сторінки"
simple_title:         "Завантаження веб-сторінки"
programming_language: "C++"
category:             "C++"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/cpp/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Що і для чого?

Скачування веб-сторінки означає отримання всієї інформації з цієї сторінки і збереження її на нашому комп'ютері. Програмісти часто роблять це, щоб аналізувати дані або скраплювати інформацію з веб-сторінок.

## Як це зробити:

```C++
#include <iostream>
#include <curl/curl.h>

int main()
{
    // Створюємо об'єкт CURL
    CURL *curl;
    curl = curl_easy_init();
    
    if (curl) {
        // Вказуємо URL для завантаження
        curl_easy_setopt(curl, CURLOPT_URL, "https://www.example.com");
        
        // Виконуємо запит і зберігаємо результати у змінну
        CURLcode res;
        res = curl_easy_perform(curl);
        
        // Виводимо результати на екран
        std::cout << res << std::endl;
        
        // Звільнюємо пам'ять
        curl_easy_cleanup(curl);
    }
    
    return 0;
}
```

Результат:
```
200
```

## Глибоке занурення:

Скачування веб-сторінок давно є важливою частиною програмування. Дещо складніші способи включають в себе використання бібліотек, а також аналіз HTML-коду для отримання конкретної інформації з сторінки. Також існують альтернативні засоби, такі як використання API або утиліти командного рядка для скачування веб-сторінок.

## Дивіться також:

- [Офіційна документація CURL](https://curl.haxx.se/libcurl/)
- [Стаття про способи скачування веб-сторінок на StackOverflow](https://stackoverflow.com/questions/8567114/how-to-download-a-web-page-in-c)