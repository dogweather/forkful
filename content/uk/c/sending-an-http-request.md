---
title:                "Надсилання http запиту"
html_title:           "C: Надсилання http запиту"
simple_title:         "Надсилання http запиту"
programming_language: "C"
category:             "C"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/c/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Що і чому?

Відправлення HTTP-запиту - це процес, за допомогою якого програміст запитує дані з іншого веб-сервера. Це необхідно для отримання актуальної інформації або виклику певної функціональності, яка недоступна на поточному сервері.

## Як:

```C
#include <stdio.h>
#include <curl/curl.h>

int main(void)
{
  CURL *curl;
  CURLcode res;
  
  // створення нового об'єкта CURL
  curl = curl_easy_init();
  
  if(curl) {
    // налаштування URL-адреси, на яку буде відправлений запит
    curl_easy_setopt(curl, CURLOPT_URL, "https://example.com");
    
    // виконання запиту
    res = curl_easy_perform(curl);
    
    // перевірка на помилки
    if(res != CURLE_OK)
      fprintf(stderr, "curl error: %s\n", curl_easy_strerror(res));
      
    // закриття з'єднання
    curl_easy_cleanup(curl);
  }
  
  return 0;
}
```

Вивід:

```
<!doctype html>
<html>
<head>
<title>Example Domain</title>
<meta charset="utf-8" />
<meta http-equiv="Content-type" content="text/html; charset=utf-8" />
<meta name="viewport" content="width=device-width, initial-scale=1" />
<style type="text/css">
body {
  background-color: #f0f0f2;
...

```

## Детальний розгляд:

Відправлення HTTP-запиту стало необхідним з появою веб-серверів і їхньої загальної доступності для отримання та передачі даних. Протягом років з'явилося багато інструментів для взаємодії з веб-серверами, наприклад, бібліотеки libcurl, яку ми використовуємо у прикладі. Існують також інші альтернативи, такі як використання сокетів або спеціалізовані бібліотеки для роботи зі специфічними видами запитів, наприклад, REST або GraphQL. Отримання даних з інших серверів є важливою частиною веб-розробки, тому важливо мати розуміння процесу відправлення HTTP-запитів.

## Додаткові матеріали:

- Офіційна документація бібліотеки libcurl: https://curl.haxx.se/libcurl/
- Порівняння спеціалізованих бібліотек для роботи зі специфічними видами запитів: https://www.getpostman.com/docs/v6/postman/sending_api_requests/comparing_libraries
- Далі про використання сокетів у C: https://www.geeksforgeeks.org/socket-programming-in-cc-handling-multiple-clients-on-server-without-multi-threading/
- Основи REST та GraphQL: https://www.freecodecamp.org/news/rest-graphql-apis-explained/