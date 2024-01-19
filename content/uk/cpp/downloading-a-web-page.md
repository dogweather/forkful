---
title:                "Завантаження веб-сторінки"
html_title:           "Gleam: Завантаження веб-сторінки"
simple_title:         "Завантаження веб-сторінки"
programming_language: "C++"
category:             "C++"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/cpp/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Що й для чого?
Завантаження веб-сторінки - це процес отримання даних з веб-сайту і зберігання цих даних на локальному пристрої. Програмісти це роблять, щоб аналізувати зміст сайту або для використання на початковому етапі веб-скрапінгу.

## Як це зробити:
Ми використаємо бібліотеку libcurl у C++ для завантаження веб-сторінки. 

```C++
#include <curl/curl.h>
#include <string>
#include <iostream>

size_t WriteData(void* buffer, size_t size, size_t nmemb, std::string* userp) {
    userp->append((char*)buffer, size * nmemb);
    return size * nmemb;
}

int main(void) {
    CURL* curl;
    CURLcode res;

    curl_global_init(CURL_GLOBAL_DEFAULT);
    curl = curl_easy_init();
    if(curl) {
        std::string readBuffer;

        curl_easy_setopt(curl, CURLOPT_URL, "http://example.com");
        curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, WriteData);
        curl_easy_setopt(curl, CURLOPT_WRITEDATA, &readBuffer);
        
       res = curl_easy_perform(curl);
     
       if(CURLE_OK == res) {
            std::cout << readBuffer << std::endl;
       }
       
        curl_easy_cleanup(curl);
    }
    
    curl_global_cleanup();
    return 0;
}
```

## Поглиблення:
1. **Історичний контекст**: libcurl був вперше випущений в 1998 році і з того часу визнаний найбільш надійним і гнучким інструментом для передачі даних в мережі через C++. 
2. **Альтернативи**: Більше сучасні альтернативи libcurl включають библіотеки, такі як Boost.Asio і cpp-netlib.
3. **Деталі виконання**: libcurl спочатку запитує DNS-сервер, переконуючись, що URL, який ви надали, дійсний. Після цього він встановлює з'єднання з сервером і запитує веб-сторінку, яку ви хотіли завантажити. 

## Дивіться також:
1. [libcurl Tutorial](https://curl.se/libcurl/c/libcurl-tutorial.html)
2. [Boost.Asio](https://www.boost.org/doc/libs/1_77_0/doc/html/boost_asio.html)
3. [cpp-netlib](https://cpp-netlib.github.io/)