---
title:                "C++: Відправлення http-запиту з базовою автентифікацією."
simple_title:         "Відправлення http-запиту з базовою автентифікацією."
programming_language: "C++"
category:             "C++"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/cpp/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Чому
Запит HTTP з базовою аутентифікацією є важливою технікою у веб-програмуванні, яка забезпечує безпеку обміну даними з сервером.

## Як
```C++
// Створення об'єкта запиту
QNetworkRequest request(url);

// Додавання заголовка з даними для аутентифікації
QString auth = "Basic " + QByteArray(QString("%1:%2").arg(username).arg(password).toUtf8()).toBase64();
request.setRawHeader("Authorization", auth.toUtf8());

// Виконання запиту та отримання відповіді
QNetworkAccessManager manager;
QNetworkReply *reply = manager.get(request);
reply->waitForReadyRead();
QByteArray responseData = reply->readAll();

// Виведення результату
qDebug() << responseData;
```

Приклад виходу:
```
<HTTP/1.1 200 OK
<Data from server>
```

## Глибоке дослідження
Аутентифікація - процес перевірки ідентифікаційних даних користувача для доступу до обмеженої функціональності або ресурсів. Базова аутентифікація є одним з найпростіших методів аутентифікації, який передбачає передачу користувачем свого імені користувача та пароля через HTTP заголовок Authorization. Це дозволяє серверу перевірити дійсність ідентифікаційних даних і надати доступ до потрібних ресурсів.

## Дивіться також
- [Документація Qt для класу QNetworkRequest](https://doc.qt.io/qt-5/qnetworkrequest.html)
- [Поради по безпечності з аутентифікації](https://developer.mozilla.org/uk/docs/Web/Security/Authentication)
- [Стаття про базову аутентифікацію в HTTP](https://www.httpwatch.com/http-gallery/authentication/)