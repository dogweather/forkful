---
title:                "C: Завантаження веб-сторінки"
simple_title:         "Завантаження веб-сторінки"
programming_language: "C"
category:             "C"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/c/downloading-a-web-page.md"
---

{{< edit_this_page >}}

# Зачем

Завантаження веб-сторінки може бути необхідним для багатьох різноманітних речей у програмуванні на C, включаючи отримання корисної інформації з Інтернету, роботу зі зображеннями або просто для вивчення технологій роботи з мережами.

# Як

Для початку завантаження сторінки нам потрібно знайти URL-адресу сторінки, яку ми хочемо завантажити. Це можна зробити за допомогою функції `gethostbyname()`, яка зчитує інформацію про хост з допомогою DNS. Приклад коду:

```C
char *url = "https://example.com";
struct hostent *website = gethostbyname(url);
```

Потім ми можемо використовувати функцію `connect()`, щоб встановити з'єднання з сервером. Після цього ми можемо передати HTTP-запит на завантаження сторінки. Приклад коду:

```C
int connection = socket(AF_INET, SOCK_STREAM, 0);
struct sockaddr_in server_addr;
server_addr.sin_family = AF_INET;
server_addr.sin_addr.s_addr = INADDR_ANY;
server_addr.sin_port = htons(80);
connect(connection, (struct sockaddr*)&server_addr, sizeof(server_addr));
char *request = "GET / HTTP/1.1\r\nHost: example.com\r\n\r\n";
send(connection, request, strlen(request), 0);
```

Після того, як ми відправили запит, ми можемо отримати відповідь від сервера у вигляді байтів, які потрібно обробити і вивести у зрозумілому форматі. Приклад коду:

```C
char buffer[1024];
recv(connection, buffer, sizeof(buffer), 0);
printf("%s", buffer);
```

В результаті ви отримаєте вивід сторінки у терміналі.

# Глибші деталі

В цьому прикладі ми завантажили лише базову сторінку з HTTP-заголовком `GET / HTTP/1.1`. Але існує багато інших методів і типів запитів, які можуть бути корисними для різних веб-комунікацій. Також важливо пам'ятати про обробку помилок і використання захисних механізмів при роботі з мережами.

# Дивись також

- [Офіційна документація з функцій для мережевого програмування у C](http://man7.org/linux/man-pages/man3/index.html#name_section)
- [Приклади коду для завантаження сторінок за допомогою C](https://www.geeksforgeeks.org/downloading-a-webpage-using-urllib-module/)
- [Поради по роботі з мережевими даними у C](https://www.gnu.org/software/libc/manual/html_node/Network-Addresses.html#Network-Addresses)