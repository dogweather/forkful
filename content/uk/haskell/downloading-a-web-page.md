---
title:                "Завантаження веб-сторінки"
html_title:           "Haskell: Завантаження веб-сторінки"
simple_title:         "Завантаження веб-сторінки"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/haskell/downloading-a-web-page.md"
---

{{< edit_this_page >}}

Що & Чому?
Завантаження веб-сторінки означає отримання інформації з Інтернету. Програмісти часто роблять це для отримання даних, які потрібні для їх програм або для аналізу та обробки вмісту веб-сторінки.

Як зробити:
```Haskell
import Network.HTTP.Simple

main = do

-- Завантаження веб-сторінки:
response <- httpLBS "https://www.google.com"

-- Виведення статусу підключення:
print $ getResponseStatusCode response

-- Виведення вмісту сторінки:
print $ getResponseBody response
```

Результат:
```Haskell
200
<!doctype html> ...
```

Глибокий погляд:
- Завантаження веб-сторінки можна зробити за допомогою багатьох інструментів, таких як бібліотека Network.HTTP.Simple, яку ми показали в прикладі.
- Існують також інші способи отримання даних з Інтернету, наприклад, за допомогою програми wget або використанням API веб-серверів.
- У реалізації завантаження веб-сторінки є багато деталей, таких як обробка помилок, робота зі змінними хостів, кешування тощо.

Див. також:
- [Офіційна документація Haskell](https://www.haskell.org/documentation/)
- [Бібліотека Network.HTTP.Simple на Hackage](https://hackage.haskell.org/package/http-client-0.7.3.1/docs/Network-HTTP-Simple.html)
- [Туторіал по завантаженню веб-сторінок на Сodecademy](https://www.codecademy.com/en/courses/web-beginner-en-LkL9Q/0/1)