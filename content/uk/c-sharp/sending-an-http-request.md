---
title:                "C#: Надсилання запиту http"
simple_title:         "Надсилання запиту http"
programming_language: "C#"
category:             "C#"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/c-sharp/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Чому

Надсилання HTTP-запитів є необхідною частиною багатьох програм та веб-сайтів. Це дозволяє нам отримати та обмінюватися даними з іншими серверами. Незалежно від того, чи ви розробляєте мобільний додаток, створюєте веб-сайт або працюєте з програмою, яка потребує зв'язку з Інтернетом, ви маєте зрозуміти, як відправити HTTP-запит.

## Як це зробити

Щоб надіслати HTTP-запит з нашої програми, ми використовуємо клас WebClient з простору імен System.Net. У наступному прикладі ми надішлемо GET-запит на веб-сайт "https://www.example.com" та виведемо відповідь сервера.

```C#
WebClient client = new WebClient();
string response = client.DownloadString("https://www.example.com");
Console.WriteLine(response);
```

Ви отримаєте результат у вигляді HTML-коду сторінки "https://www.example.com" у вашому виводі консолі.

## Вглиб

Надсилання HTTP-запитів включає в себе використання різних методів, таких як GET, POST, PUT, DELETE та інші. Крім того, у нас є можливість додавати заголовки та параметри до наших запитів. Наприклад, у нашому попередньому прикладі ми могли би додати заголовок "User-Agent", щоб ідентифікувати наш додаток під час відправки запиту.

```C#
client.Headers.Add("User-Agent", "MyApp/1.0");
```

Також, щоб отримати більш детальну інформацію про відповідь сервера, ми можемо використовувати клас HttpWebRequest та отримати об'єкт HttpWebResponse з методу GetResponse.

```C#
HttpWebRequest request = (HttpWebRequest)WebRequest.Create("https://www.example.com");
HttpWebResponse response = (HttpWebResponse)request.GetResponse();
Console.WriteLine("Status code: " + response.StatusCode);
```

Ви отримаєте код статусу 200, який означає успішне виконання запиту.

## Дивись також

- [Робота з HTTP-запитами в C#](https://docs.microsoft.com/uk-ua/dotnet/csharp/tutorials/intro-to-httpclient)
- [Уроки з C# для початківців](https://www.tutorialspoint.com/csharp/index.htm)
- [Поради та рекомендації з програмування в C#](https://www.c-sharpcorner.com/)