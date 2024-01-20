---
title:                "Перетворення дати в рядок"
html_title:           "Lua: Перетворення дати в рядок"
simple_title:         "Перетворення дати в рядок"
programming_language: "Swift"
category:             "Swift"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/swift/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Що і чому?
Перетворення дати в рядок - це процес форматування об'єкту дати до текстового формату. Програмісти роблять це, щоб легко відобразити та використовувати дати у вигляді тексту.

## Як це зробити:
Створимо дату та перетворимо її на рядок у Swift. Просто скористайтесь кодом нижче:

```Swift
    let currentDate = Date()
    let formatter = DateFormatter()
    formatter.dateFormat = "yyyy-MM-dd"
    let dateString = formatter.string(from: currentDate)
    print(dateString)
```
В результаті на екран виведеться поточна дата в форматі "yyyy-MM-dd".

## Глибше дослідження:
Перетворення дати в рядок було необхідним із зародженням комп’ютерного програмування. Було багато методів виконання цього завдання, але Swift надає елегантний підхід за допомогою класу `DateFormatter`.

Натомість, є альтернативні способи працювати із датами, такі як часові штампи Unix, які представляють час як кількість секунд, що сплили від 1 січня 1970 року.

Якщо хочете глибше зрозуміти, як працює `DateFormatter`, то він просто бере шаблон дати/часу та застосовує його до об'єкта дати, тим самим генеруючи рядок.

## Див. також:
1. [Офіційна документація Apple про DateFormatter](https://developer.apple.com/documentation/foundation/dateformatter)
2. [Вступ до обробки дат та часу в Swift](https://www.hackingwithswift.com/articles/141/working-with-dates-and-times-in-swift)
3. [Керівництво по Unix часовим штампам](http://pubs.opengroup.org/onlinepubs/9699919799/basedefs/V1_chap04.html#tag_04_16)