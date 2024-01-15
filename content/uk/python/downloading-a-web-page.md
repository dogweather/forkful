---
title:                "Завантаження веб-сторінки"
html_title:           "Python: Завантаження веб-сторінки"
simple_title:         "Завантаження веб-сторінки"
programming_language: "Python"
category:             "Python"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/python/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Чому 

В сучасному світі, коли доступ до інформації став дуже простим і швидким, завантаження веб-сторінок може дати нам необхідну інформацію або допомогти знайти потрібний продукт або послугу.

## Як це зробити 

```Python 
import requests  

url = "https://www.example.com" # замініть посилання на потрібну веб-сторінку 

response = requests.get(url) 

print(response.status_code) # вивести статус-код відповіді сервера 
print(response.content) # вивести вміст сторінки у текстовому форматі 
``` 

Ви можете також використовувати бібліотеку BeautifulSoup для парсингу HTML-коду сторінки і отримання конкретних даних. 

## Глибокий погляд 

Завантаження веб-сторінки за допомогою Python дозволяє нам не тільки отримати вміст сторінки, але і здійснювати дії, які зазвичай роблять користувачі, наприклад, заповнити форму або виконати пошук. Також це може бути корисно для автоматизації завантаження даних з декількох веб-сторінок. 

## Дивіться також 

- [Офіційна документація з бібліотеки requests](https://requests.readthedocs.io/en/master/) 
- [Повний список методів GET, POST, PUT, DELETE у HTTP-запитах](https://www.w3schools.com/tags/ref_httpmethods.asp) 
- [Офіційна документація з бібліотеки BeautifulSoup](https://www.crummy.com/software/BeautifulSoup/bs4/doc/)