---
title:                "Відправлення HTTP-запиту з базовою аутентифікацією"
aliases:
- /uk/vba/sending-an-http-request-with-basic-authentication/
date:                  2024-02-01T22:03:36.768740-07:00
model:                 gpt-4-0125-preview
simple_title:         "Відправлення HTTP-запиту з базовою аутентифікацією"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/vba/sending-an-http-request-with-basic-authentication.md"
changelog:
  - 2024-02-01, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Що та чому?

Відправлення HTTP-запиту з базовою автентифікацією у Visual Basic for Applications (VBA) полягає в доступі до веб-ресурсів, що захищені іменем користувача та паролем. Програмісти роблять це для взаємодії з захищеними API або веб-сервісами у своїх застосунках на VBA, наприклад, для автоматизації завдань у Excel або Access із даними з захищених кінцевих точок.

## Як це зробити:

У VBA ви можете використовувати бібліотеку `Microsoft XML, v6.0` (MSXML2) для відправлення HTTP-запитів із базовою автентифікацією. Це передбачає встановлення заголовка `"Authorization"` запиту для включення облікових даних у форматі, закодованому за допомогою base64. Ось покроковий посібник:

1. **Додайте посилання на MSXML2**: Спочатку переконайтеся, що ваш проєкт VBA посилається на бібліотеку `Microsoft XML, v6.0`. У редакторі VBA перейдіть до Tools > References та позначте `Microsoft XML, v6.0`.

2. **Створіть та надішліть HTTP-запит**: Використовуйте наступний фрагмент коду VBA як приклад. Замініть `"your_username"` та `"your_password"` на свої справжні облікові дані та відкоригуйте URL за потребою.

    ```vb
    Dim XMLHttp As Object
    Set XMLHttp = CreateObject("MSXML2.XMLHTTP")
    Dim url As String
    url = "http://example.com/api/resource" ' Замініть на справжній URL
    Dim base64Credentials As String
    base64Credentials = EncodeBase64("your_username:your_password")
    
    XMLHttp.Open "GET", url, False
    XMLHttp.setRequestHeader "Authorization", "Basic " & base64Credentials
    XMLHttp.send
    
    Debug.Print XMLHttp.responseText ' Виводить відповідь у вікно Immediate
    ```

3. **Кодування облікових даних у форматі base64**: VBA не має вбудованої функції для кодування у форматі base64, але ви можете використовувати цю користувацьку функцію `EncodeBase64`:

    ```vb
    Function EncodeBase64(text As String) As String
        Dim arrData() As Byte
        arrData = StrConv(text, vbFromUnicode)
        
        Dim objXML As MSXML2.DOMDocument60
        Dim objNode As MSXML2.IXMLDOMElement
        
        Set objXML = New MSXML2.DOMDocument60
        Set objNode = objXML.createElement("b64")
        
        objNode.dataType = "bin.base64"
        objNode.nodeTypedValue = arrData
        EncodeBase64 = objNode.Text
    End Function
    ```
    
Це відправить GET-запит на `http://example.com/api/resource` із зазначеними обліковими даними для базової автентифікації та виведе відповідь.

## Поглиблений аналіз

Використовуваний тут підхід, хоча і ефективний для простих випадків використання, опирається на схему базової автентифікації, яка відправляє облікові дані у форматі, легко декодованому (кодування base64 не є шифруванням). Через свою вразливість, особливо в контекстах без HTTPS, базова автентифікація не рекомендується для передачі конфіденційних даних через Інтернет без додаткових шарів безпеки, таких як SSL/TLS.

Історично базова автентифікація була одним із перших методів, розроблених для контролю доступу до веб-ресурсів. Сьогодні для нових застосунків загалом віддають перевагу безпечнішим та більш гнучким стандартам автентифікації, таким як OAuth 2.0. Враховуючи обмеження VBA та зовнішні залежності, потрібні для складніших методів автентифікації, розробники часто використовують VBA у внутрішніх або менш критичних з точки зору безпеки середовищах або як крок для швидкого прототипування ідей.

При використанні VBA для HTTP-запитів пам'ятайте, що кожна версія бібліотеки MSXML може підтримувати різні функції та стандарти безпеки. Завжди використовуйте найновішу версію, сумісну з вашим застосунком, щоб забезпечити кращу безпеку та продуктивність. Крім того, враховуйте обмеження середовища та потенційно застарілі функції при виборі VBA для нових проєктів, особливо тих, що вимагають захищених HTTP-спілкувань. Інші програмні середовища або мови можуть пропонувати більш надійні, безпечні та зручні для обслуговування рішення для подібних завдань.
