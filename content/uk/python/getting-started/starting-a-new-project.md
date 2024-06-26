---
changelog:
- 2024-02-22, dogweather, reviewed
- 2024-02-22, gpt-4-0125-preview, translated from English
date: 2024-02-22 17:30:14.217754-07:00
description: "\u042F\u043A \u0446\u0435 \u0437\u0440\u043E\u0431\u0438\u0442\u0438\
  : \u0412\u0456\u0440\u0442\u0443\u0430\u043B\u044C\u043D\u0435 \u0441\u0435\u0440\
  \u0435\u0434\u043E\u0432\u0438\u0449\u0435 \u2014 \u0446\u0435 \u0441\u0430\u043C\
  \u043E\u0441\u0442\u0456\u0439\u043D\u0430 \u043F\u0430\u043F\u043A\u0430, \u044F\
  \u043A\u0430 \u043C\u0456\u0441\u0442\u0438\u0442\u044C \u0443\u0441\u0456 \u043D\
  \u0435\u043E\u0431\u0445\u0456\u0434\u043D\u0456 \u0432\u0438\u043A\u043E\u043D\u0443\
  \u0432\u0430\u043D\u0456 \u0444\u0430\u0439\u043B\u0438 \u0434\u043B\u044F \u0432\
  \u0438\u043A\u043E\u0440\u0438\u0441\u0442\u0430\u043D\u043D\u044F \u043F\u0430\u043A\
  \u0435\u0442\u0456\u0432, \u044F\u043A\u0456 \u043C\u043E\u0436\u0443\u0442\u044C\
  \ \u0437\u043D\u0430\u0434\u043E\u0431\u0438\u0442\u0438\u0441\u044F\u2026"
lastmod: '2024-03-13T22:44:48.584132-06:00'
model: gpt-4-0125-preview
summary: "\u0412\u0456\u0440\u0442\u0443\u0430\u043B\u044C\u043D\u0435 \u0441\u0435\
  \u0440\u0435\u0434\u043E\u0432\u0438\u0449\u0435 \u2014 \u0446\u0435 \u0441\u0430\
  \u043C\u043E\u0441\u0442\u0456\u0439\u043D\u0430 \u043F\u0430\u043F\u043A\u0430\
  , \u044F\u043A\u0430 \u043C\u0456\u0441\u0442\u0438\u0442\u044C \u0443\u0441\u0456\
  \ \u043D\u0435\u043E\u0431\u0445\u0456\u0434\u043D\u0456 \u0432\u0438\u043A\u043E\
  \u043D\u0443\u0432\u0430\u043D\u0456 \u0444\u0430\u0439\u043B\u0438 \u0434\u043B\
  \u044F \u0432\u0438\u043A\u043E\u0440\u0438\u0441\u0442\u0430\u043D\u043D\u044F\
  \ \u043F\u0430\u043A\u0435\u0442\u0456\u0432, \u044F\u043A\u0456 \u043C\u043E\u0436\
  \u0443\u0442\u044C \u0437\u043D\u0430\u0434\u043E\u0431\u0438\u0442\u0438\u0441\u044F\
  \ \u043F\u0440\u043E\u0454\u043A\u0442\u0443 Python."
title: "\u041F\u043E\u0447\u0430\u0442\u043E\u043A \u043D\u043E\u0432\u043E\u0433\u043E\
  \ \u043F\u0440\u043E\u0454\u043A\u0442\u0443"
weight: 1
---

## Як це зробити:


### Створення віртуального середовища
Віртуальне середовище — це самостійна папка, яка містить усі необхідні виконувані файли для використання пакетів, які можуть знадобитися проєкту Python. Рекомендується створювати віртуальне середовище для кожного проєкту, щоб уникнути конфліктів між залежностями проєктів. Використовуйте модуль `venv`, який є частиною стандартної бібліотеки Python.

```shell
# Замініть 'myproject' на ім'я вашого проєкту
python3 -m venv myproject-env
```

Щоб активувати віртуальне середовище:

На Windows:
```shell
myproject-env\Scripts\activate.bat
```

На Unix або MacOS:
```shell
source myproject-env/bin/activate
```

Приклад виводу (вивід може дещо відрізнятися залежно від ОС):
```shell
(myproject-env) $
```

### Встановлення пакетів
Використовуйте `pip`, інсталятор пакетів для Python, щоб встановлювати, оновлювати та видаляти пакети. Ось як ви можете встановити популярну сторонню бібліотеку `requests`, щоб робити HTTP-запити:

```shell
pip install requests
```

Приклад виводу:
```shell
Collecting requests
  Downloading requests-2.25.1-py2.py3-none-any.whl (61 kB)
     |████████████████████████████████| 61 kB 1.3 MB/s
Installing collected packages: requests
Successfully installed requests-2.25.1
```

### Налаштування структури проєкту
Типовий проєкт на Python може виглядати приблизно так:

```
myproject/
│
├── myproject-env/    # Віртуальне середовище
├── docs/             # Документація
├── tests/            # Модульні та інтеграційні тести
│   └── __init__.py
├── myproject/        # Вихідний код проєкту
│   ├── __init__.py
│   └── main.py
├── setup.py          # Файл налаштування проєкту
└── README.md         # Огляд проєкту
```

### Створіть свою першу програму
Створіть файл `main.py` всередині папки `myproject`. Ось приклад простої програми:

```python
# myproject/myproject/main.py
def greet(name):
    return f"Привіт, {name}!"

if __name__ == "__main__":
    print(greet("Світе"))
```

Запустіть свою програму:

```shell
python myproject/main.py
```

Приклад виводу:
```shell
Привіт, Світе!
```

### Використовуйте фреймворк для більших проєктів
Для більших проєктів, особливо веб-додатків, фреймворки на кшталт Django або Flask є незамінними. Ось як встановити Flask та створити простий веб-додаток "Привіт, Світе":

```shell
pip install Flask
```

Створіть файл `app.py` з наступним вмістом:

```python
# app.py
from flask import Flask
app = Flask(__name__)

@app.route("/")
def hello_world():
    return "<p>Привіт, Світе!</p>"

if __name__ == "__main__":
    app.run(debug=True)
```

Запустіть Flask-додаток:

```shell
flask run
```

Приклад виводу:
```shell
 * Running on http://127.0.0.1:5000/ (Press CTRL+C to quit)
```

Перейдіть за адресою `http://127.0.0.1:5000/` у своєму веб-браузері, і ви повинні побачити повідомлення "Привіт, Світе!".
