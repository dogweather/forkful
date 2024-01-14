---
title:                "Python: Завантаження веб-сторінки"
simple_title:         "Завантаження веб-сторінки"
programming_language: "Python"
category:             "Python"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/python/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Чому

Використання програмування мовою Python може бути корисним для багатьох різних цілей, включаючи завантаження веб-сторінок. Це може бути зручним для отримання даних для аналізу, створення скрейпера для інтернет-магазину або просто для того, щоб отримати необхідну інформацію швидко та зручно.

## Як зробити

Для початку необхідно імпортувати бібліотеку `requests`, яка дозволить зробити запит до веб-сторінки. Приклад коду для завантаження сторінки і отримання її змісту виглядає наступним чином:

```Python
import requests

url = "https://www.python.org"
response = requests.get(url)
print(response.text)
```

У цьому прикладі ми завантажуємо сторінку Python.org та виводимо її HTML-код. Звичайно, можна також зберегти цей вміст у змінну або зберегти його у файл.

Щоб вказати певні ресурси чи параметри запиту, можна також використовувати параметри `params` та `headers`:

```Python
import requests

url = "https://www.python.org/search/"
params = {
    "q": "python",
    "submit": "Search"
}
headers = {"User-Agent": "Mozilla/5.0"}
response = requests.get(url, params=params, headers=headers)
print(response.text)
```

У цьому прикладі ми шукаємо слово "python" на сторінці пошуку Python.org та використовуємо підроблений User-Agent (браузер), щоб уникнути блокування нашого запиту.

## Вдаваймося в деталі

Завантаження веб-сторінки не обмежується лише отриманням її HTML-коду. Можна також витягувати певні дані зі сторінки за допомогою бібліотеки `BeautifulSoup`. Наприклад, якщо на веб-сторінці є таблиця з даними, можна витягнути ці дані у вигляді Python-списку за допомогою наступного коду:

```Python
import requests
from bs4 import BeautifulSoup

url = "https://www.example.com/table"
response = requests.get(url)
soup = BeautifulSoup(response.text, "lxml")
table = soup.find("table")
data = []
for row in table.find_all("tr"):
    cells = []
    for cell in row.find_all("td"):
        cells.append(cell.text)
    data.append(cells)
print(data)
```

Також варто зазначити, що використання автоматизації завантаження веб-сторінок повинно бути виконане з відповідними морально-етичними переживаннями та згідно з правилами сторінки, яку ви намагаєтеся завантажити.

## Дивіться також

- [Документація бібліотеки requests](https://requests.readthedocs.io/en/latest/)
- [Документація біб