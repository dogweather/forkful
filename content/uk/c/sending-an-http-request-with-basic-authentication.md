---
title:                "Відправка http-запиту з базовою аутентифікацією"
html_title:           "C: Відправка http-запиту з базовою аутентифікацією"
simple_title:         "Відправка http-запиту з базовою аутентифікацією"
programming_language: "C"
category:             "C"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/c/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

Що і чому?

В програмуванні часто використовується відправлення HTTP-запитів з основною аутентифікацією. Це означає, що наш код буде передавати основну аутентифікаційну інформацію (ім'я користувача та пароль) разом із запитом, щоб сервер міг перевірити нашу ідентичність та дозволити доступ до ресурсів. Це дозволяє нам захистити конфіденційну інформацію та виконувати безпечні запити.

Як це зробити:

```c
#include <stdio.h>
#include <curl/curl.h>

int main(void)
{
  CURL *curl;
  CURLcode res;

  curl = curl_easy_init();
  if(curl) {
    curl_easy_setopt(curl, CURLOPT_URL, "https://example.com/api/");
    // Встановлюємо основну аутентифікацію зі своїми даними
    curl_easy_setopt(curl, CURLOPT_HTTPAUTH, CURLAUTH_BASIC);
    curl_easy_setopt(curl, CURLOPT_USERPWD, "username:password");

    res = curl_easy_perform(curl);

    /* Перевіряємо код відповіді */
    if(res != CURLE_OK)
      fprintf(stderr, "curl_easy_perform() failed: %s\n",
              curl_easy_strerror(res));

    /* Завершуємо сеанс */
    curl_easy_cleanup(curl);
  }
  return 0;
}
```

Студія curl/easy та бібліотека libcurl надають нам зручні функції для роботи з HTTP запитами. Ми встановлюємо кодування аутентифікації CURLAUTH_BASIC та передаємо його разом з ім'ям користувача та паролем за допомогою функції CURLOPT_USERPWD. Останній крок - виконати запит за допомогою функції curl_easy_perform. Очікуваний результат повинен бути успішним (0). 

Глибоке занурення:

HTTP-основна аутентифікація виникла в 90-ті роки, і була єдиною методом аутентифікації для HTTP протоколу до 2010 року. Однак, вона все ще може бути використана в багатьох випадках, де потрібно просте та ефективне забезпечення аутентифікації. 

Можливу альтернативою основній аутентифікації є Digest аутентифікація, яка є більш безпечною, але більш важкою в застосуванні. Також існує можливість використання бібліотеки OpenSSL для основної аутентифікації за допомогою функцій SSL.

Дивитися також:

- Ofіційна документація libcurl: https://curl.se/libcurl/c/http-auth.html
- GitHub репозиторій libcurl: https://github.com/curl/curl