---
title:                "Надсилання http-запиту з основною аутентифікацією."
html_title:           "Go: Надсилання http-запиту з основною аутентифікацією."
simple_title:         "Надсилання http-запиту з основною аутентифікацією."
programming_language: "Go"
category:             "Go"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/go/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Що і Чому?
Відправка запиту HTTP з базовою аутентифікацією - це процес, коли програмісти використовують HTTP протокол для посилання запитів до сервера за допомогою особистого ідентифікатора і пароля. Це необхідно для доступу до захищених ресурсів, таких як сторінки з обмеженим доступом або API.

## Як це зробити:
```Go
import "net/http"

func main() {
    // Створення основного запиту з потрібною URL
    req, err := http.NewRequest("GET", "https://example.com/api/users", nil)
    
    // Додавання базової аутентифікації до запиту
    req.SetBasicAuth("username", "password")
    
    // Відправлення запиту і отримання відповіді
    client := &http.Client{}
    resp, err := client.Do(req)

    // Перевірка помилок та читання відповіді
    if err != nil {
        fmt.Println("Помилка при відправці запиту:", err)
    } else {
        body, _ := ioutil.ReadAll(resp.Body)
        fmt.Println("Отримано відповідь:", string(body))
    }
}
```

## Глибоке погруження:
- Історичний контекст: базова аутентифікація була встановлена як стандартний метод аутентифікації для HTTP протоколу ще в 1999 році.
- Альтернативи: на сьогоднішній день, існує багато інших методів аутентифікації, таких як OAuth або JWT.
- Деталі реалізації: більш докладну інформацію з можливостями та обмеженнями базової аутентифікації ви можете знайти в документації Go.

## Дивись також:
- [Документація Go](https://golang.org/pkg/net/http/#Request.SetBasicAuth)
- [Стаття на тему аутентифікації в Go](https://itnext.io/authentication-and-authorization-using-jwt-with-golang-ee435ebb6efe)
- [Офіційна специфікація HTTP базової аутентифікації](https://tools.ietf.org/html/rfc7617)