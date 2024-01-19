---
title:                "Надсилання http-запиту"
html_title:           "Arduino: Надсилання http-запиту"
simple_title:         "Надсилання http-запиту"
programming_language: "C"
category:             "C"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/c/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Що і Навіщо?

Надсилання HTTP-запиту - це процес відправки запиту до веб-серверу для завантаження веб-сторінки чи отримання даних. Програмісти це роблять, коли їм потрібно взаємодіяти з веб-сервісами або API.

## Як це робити:

У фрагменті коду нижче наведено використання бібліотеки libcurl для надсилання HTTP-запиту.

```C
#include <stdio.h>
#include <curl/curl.h>

int main(void)
{
  CURL *curl;
  CURLcode res;

  curl = curl_easy_init();
  if(curl) {
    curl_easy_setopt(curl, CURLOPT_URL, "http://example.com");

    res = curl_easy_perform(curl);

    if(res != CURLE_OK)
      fprintf(stderr, "curl_easy_perform() failed: %s\n",
              curl_easy_strerror(res));

    curl_easy_cleanup(curl);
  }
  return 0;
}
```

При виконанні цього коду, якщо все пройде успішно, відбудеться HTTP-запит до example.com.

## Поглиблено

HTTP-запити використовуються з самого початку інтернету – вони є основою передачі даних через веб. Хоча є інші протоколи, такі як FTP і SMTP, HTTP домінує, коли мова йде про взаємодію з веб-сайтами і API.

Є багато способів надсилати HTTP-запити в C. Наведений раніше код використовує libcurl, відому бібліотеку для взаємодії з URL. Але є і інші бібліотеки, наприклад, POCO і Boost.Asio.

Важливо розуміти, що libcurl використовує низькорівневі системні виклики для встановлення з'єднання, надсилання та отримання даних. Код вище тільки приховує цю складність.

## Див. також:

1. [Документацію libcurl](https://curl.haxx.se/libcurl/c/)
2. [POCO бібліотека](https://pocoproject.org/)
3. [Boost.Asio бібліотека](https://www.boost.org/doc/libs/1_66_0/doc/html/boost_asio.html)