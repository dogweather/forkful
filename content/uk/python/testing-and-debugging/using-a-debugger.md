---
date: 2024-01-26 04:09:26.274806-07:00
description: "\u042F\u043A \u043A\u043E\u0440\u0438\u0441\u0442\u0443\u0432\u0430\u0442\
  \u0438\u0441\u044F: \u0414\u0430\u0432\u0430\u0439\u0442\u0435 \u0440\u043E\u0437\
  \u0433\u043B\u044F\u043D\u0435\u043C\u043E \u0432\u0438\u043A\u043E\u0440\u0438\u0441\
  \u0442\u0430\u043D\u043D\u044F `pdb`, \u0432\u0431\u0443\u0434\u043E\u0432\u0430\
  \u043D\u043E\u0433\u043E \u0434\u0435\u0431\u0430\u0433\u0435\u0440\u0430 Python.\
  \ \u0423\u044F\u0432\u0456\u0442\u044C \u0444\u0430\u0439\u043B, `buggy.py`, \u0437\
  \ \u043B\u0443\u043A\u0430\u0432\u043E\u044E \u043F\u043E\u043C\u0438\u043B\u043A\
  \u043E\u044E."
lastmod: '2024-03-13T22:44:48.590906-06:00'
model: gpt-4-0125-preview
summary: "\u0414\u0430\u0432\u0430\u0439\u0442\u0435 \u0440\u043E\u0437\u0433\u043B\
  \u044F\u043D\u0435\u043C\u043E \u0432\u0438\u043A\u043E\u0440\u0438\u0441\u0442\u0430\
  \u043D\u043D\u044F `pdb`, \u0432\u0431\u0443\u0434\u043E\u0432\u0430\u043D\u043E\
  \u0433\u043E \u0434\u0435\u0431\u0430\u0433\u0435\u0440\u0430 Python."
title: "\u0412\u0438\u043A\u043E\u0440\u0438\u0441\u0442\u0430\u043D\u043D\u044F \u0434\
  \u0435\u0431\u0430\u0433\u0435\u0440\u0430"
weight: 35
---

## Як користуватися:
Давайте розглянемо використання `pdb`, вбудованого дебагера Python. Уявіть файл, `buggy.py`, з лукавою помилкою:

```Python
def add_one(number):
    result = number ++ 1
    return result

print(add_one(7))
```

Запускаючи цей скрипт, ви очікуєте побачити `8`, але отримуєте лише синтаксичну помилку. Час для дебагера!

У вашому терміналі запустіть:
```bash
python -m pdb buggy.py
```

Ви ввійдете в дебагер, і виглядатиме це так:
```Python
> /path_to_file/buggy.py(1)<module>()
-> def add_one(number):
```

Використовуйте `l(ist)` для перегляду більше коду, `n(ext)` для переходу до наступного рядка, або `c(ontinue)`, щоб продовжити виконання скрипта. Коли ви натрапите на помилку, `pdb` зупиниться і дозволить вам перевірити.

Після того, як ви виправите `number ++ 1` на `number + 1`, перезапустіть дебагер, щоб протестувати виправлення.
Пам'ятайте, кращі друзі не дозволяють друзям програмувати без мережі. Амінь.

## Поглиблене вивчення
У темні часи програмування (тобто до того, як інтегровані середовища розробки, або ІСР, були повсюдно), дебагери часто були окремими інструментами, які ви використовували поза вашим текстовим редактором. Вони приходили на допомогу, дозволяючи програмістам інспектувати стан їхнього програмного забезпечення в різних точках виконання.

Станом на 2023 рік, `pdb` Python не єдина доступна опція. Люди можуть використовувати ІСР, такі як PyCharm або Visual Studio Code, які мають свої власні просунуті дебагери. Оснащені такими зручними функціями, як точки зупину, які можна встановити кліком, замість набору криптичних команд.

Потім є `ipdb`, пакет, який можна встановити через pip, і який привносить чудові можливості `IPython` у дебагінг. Це як `pdb` на стероїдах, з доповненням автозаповнення і підсвічуванням синтаксису.

Дебагери також варіюються в своїй реалізації. Деякі працюють тісно пов'язано з виконанням програми на рівні машинного або байт-коду. Інші, як багато дебагерів високого рівня, запускають код у спеціальному середовищі, яке моніторить стан змінних і контролює потік виконання.

## Дивіться також
Для повної інформації про власний дебагер Python, перегляньте:
- Документація `pdb`: https://docs.python.org/3/library/pdb.html

Якщо ви зацікавлені в альтернативах, ці посилання будуть корисними:
- Репозиторій та керівництво використання `ipdb`: https://github.com/gotcha/ipdb
- Дебагінг за допомогою Visual Studio Code: https://code.visualstudio.com/docs/python/debugging
- Функціонал дебагінгу PyCharm: https://www.jetbrains.com/help/pycharm/debugging-code.html

Щасливого полювання на баги!
