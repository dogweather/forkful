---
title:                "Надсилаємо HTTP-запит з базової аутентифікацією"
html_title:           "C#: Надсилаємо HTTP-запит з базової аутентифікацією"
simple_title:         "Надсилаємо HTTP-запит з базової аутентифікацією"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/kotlin/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Що і чому?

Відправка HTTP-запиту з базовою аутентифікацією - це процес, коли ваше додаток встановлює віддалене з'єднання з сервером шляхом надсилання логіна і паролю. Це корисно, коли додаток потребує доступу до захищених ресурсів.

## Як це зробити:

Використовуємо бібліотеку Ktor для цього. Ось приклад коду:

```Kotlin
val client = HttpClient() {
    install(Auth) {
        basic {
            sendWithoutRequest = true
            username = "your_username"
            password = "your_password"
        }
    }
}

val response: HttpResponse = client.get("https://example.com")
println(response.status.value)
```
Зверніть увагу, що деталі для аутентифікації слід змінити на власні. Виходом буде код статусу відповіді, наприклад, `200`.

## Глибоке занурення:

Базова аутентифікація була введена у стандарт HTTP/1.0 ще у 90-ті роки. Вона проста, але також має свої недоліки, особливо крізняк безпеки.

Альтернативи включають OAuth та JWT. Вони більш складні, але надають більше функцій безпеки.

Відправка HTTP-запиту з базовою аутентифікацієєю в Kotlin включає в себе створення HttpClient та інсталяцію Auth модуля. Логін і пароль кодуються у форматі Base64, потім відправляються через заголовок `Authorization`.

## Дивіться також:

- [Довідник Ktor](https://ktor.io/)
- [Вступ до HTTP Basic Access Authentication на MDN](https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication)
- [Детальний огляд OAuth і JWT](https://www.oauth.com)