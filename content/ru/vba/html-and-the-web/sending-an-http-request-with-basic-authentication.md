---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:02:23.838871-07:00
description: "\u041A\u0430\u043A \u044D\u0442\u043E \u0441\u0434\u0435\u043B\u0430\
  \u0442\u044C: \u0412 VBA \u0432\u044B \u043C\u043E\u0436\u0435\u0442\u0435 \u0438\
  \u0441\u043F\u043E\u043B\u044C\u0437\u043E\u0432\u0430\u0442\u044C \u0431\u0438\u0431\
  \u043B\u0438\u043E\u0442\u0435\u043A\u0443 `Microsoft XML, v6.0` (MSXML2) \u0434\
  \u043B\u044F \u043E\u0442\u043F\u0440\u0430\u0432\u043A\u0438 HTTP-\u0437\u0430\u043F\
  \u0440\u043E\u0441\u043E\u0432 \u0441 \u0431\u0430\u0437\u043E\u0432\u043E\u0439\
  \ \u0430\u0443\u0442\u0435\u043D\u0442\u0438\u0444\u0438\u043A\u0430\u0446\u0438\
  \u0435\u0439. \u042D\u0442\u043E \u0432\u043A\u043B\u044E\u0447\u0430\u0435\u0442\
  \u2026"
lastmod: '2024-03-13T22:44:44.742044-06:00'
model: gpt-4-0125-preview
summary: "\u0412 VBA \u0432\u044B \u043C\u043E\u0436\u0435\u0442\u0435 \u0438\u0441\
  \u043F\u043E\u043B\u044C\u0437\u043E\u0432\u0430\u0442\u044C \u0431\u0438\u0431\u043B\
  \u0438\u043E\u0442\u0435\u043A\u0443 `Microsoft XML, v6.0` (MSXML2) \u0434\u043B\
  \u044F \u043E\u0442\u043F\u0440\u0430\u0432\u043A\u0438 HTTP-\u0437\u0430\u043F\u0440\
  \u043E\u0441\u043E\u0432 \u0441 \u0431\u0430\u0437\u043E\u0432\u043E\u0439 \u0430\
  \u0443\u0442\u0435\u043D\u0442\u0438\u0444\u0438\u043A\u0430\u0446\u0438\u0435\u0439\
  ."
title: "\u041E\u0442\u043F\u0440\u0430\u0432\u043A\u0430 HTTP-\u0437\u0430\u043F\u0440\
  \u043E\u0441\u0430 \u0441 \u0431\u0430\u0437\u043E\u0432\u043E\u0439 \u0430\u0443\
  \u0442\u0435\u043D\u0442\u0438\u0444\u0438\u043A\u0430\u0446\u0438\u0435\u0439"
weight: 45
---

## Как это сделать:
В VBA вы можете использовать библиотеку `Microsoft XML, v6.0` (MSXML2) для отправки HTTP-запросов с базовой аутентификацией. Это включает в себя установку заголовка `"Authorization"` запроса для включения учетных данных в формате, закодированном в base64. Вот пошаговое руководство:

1. **Добавление ссылки на MSXML2**: Сначала убедитесь, что ваш VBA-проект содержит ссылку на библиотеку `Microsoft XML, v6.0`. В редакторе VBA перейдите в Инструменты > Ссылки и отметьте `Microsoft XML, v6.0`.

2. **Создание и отправка HTTP-запроса**: Используйте следующий фрагмент кода VBA в качестве примера. Замените `"your_username"` и `"your_password"` на ваши фактические учетные данные и скорректируйте URL-адрес по необходимости.

    ```vb
    Dim XMLHttp As Object
    Set XMLHttp = CreateObject("MSXML2.XMLHTTP")
    Dim url As String
    url = "http://example.com/api/resource" ' Замените на фактический URL-адрес
    Dim base64Credentials As String
    base64Credentials = EncodeBase64("your_username:your_password")
    
    XMLHttp.Open "GET", url, False
    XMLHttp.setRequestHeader "Authorization", "Basic " & base64Credentials
    XMLHttp.send
    
    Debug.Print XMLHttp.responseText ' Выводит ответ в окно немедленного выполнения
    ```

3. **Кодирование учетных данных в base64**: VBA не имеет встроенной функции для кодирования в base64, но вы можете использовать эту пользовательскую функцию `EncodeBase64`:

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
    
Это отправит GET-запрос на `http://example.com/api/resource` с указанными учетными данными базовой аутентификации и выведет ответ.

## Подробнее
Используемый здесь подход, хоть и эффективный для простых случаев использования, основан на схеме базовой аутентификации, которая отправляет учетные данные в легко декодируемом формате (кодирование в base64 не является шифрованием). Из-за своей уязвимости, особенно в контекстах без HTTPS, базовая аутентификация не рекомендуется для передачи конфиденциальных данных по интернету без дополнительных слоев безопасности, таких как SSL/TLS.

Исторически базовая аутентификация была одним из первых методов, разработанных для контроля доступа к веб-ресурсам. Сегодня для новых приложений обычно предпочитают более безопасные и гибкие стандарты аутентификации, такие как OAuth 2.0. Учитывая ограничения VBA и внешние зависимости, необходимые для более продвинутых методов аутентификации, разработчики часто используют VBA во внутренних или менее критичных с точки зрения безопасности средах или используют его как промежуточный этап для быстрого прототипирования идей.

При использовании VBA для HTTP-запросов помните, что каждая версия библиотеки MSXML может поддерживать разные функции и стандарты безопасности. Всегда используйте самую последнюю версию, совместимую с вашим приложением, чтобы обеспечить лучшую безопасность и производительность. Кроме того, учитывайте ограничения среды и потенциально устаревшие функции при выборе VBA для новых проектов, особенно тех, которые требуют защищенных HTTP-соединений. Другие программные среды или языки могут предложить более надежные, безопасные и удобные в сопровождении решения для похожих задач.
