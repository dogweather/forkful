---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:07:25.083047-07:00
description: "\u042F\u043A \u0446\u0435 \u0437\u0440\u043E\u0431\u0438\u0442\u0438\
  : \u0412 \u043E\u0441\u043D\u043E\u0432\u0456 \u0441\u0432\u043E\u0457\u0439 Bash\
  \ \u0434\u043E\u0437\u0432\u043E\u043B\u044F\u0454 \u043F\u0435\u0440\u0435\u0432\
  \u0456\u0440\u044F\u0442\u0438 \u0456\u0441\u043D\u0443\u0432\u0430\u043D\u043D\u044F\
  \ \u0434\u0438\u0440\u0435\u043A\u0442\u043E\u0440\u0456\u0457, \u0432\u0438\u043A\
  \u043E\u0440\u0438\u0441\u0442\u043E\u0432\u0443\u044E\u0447\u0438 \u0443\u043C\u043E\
  \u0432\u043D\u0456 \u043E\u043F\u0435\u0440\u0430\u0442\u043E\u0440\u0438 \u0442\
  \u0430 \u043E\u043F\u0435\u0440\u0430\u0442\u043E\u0440 `-d`. \u041D\u0438\u0436\
  \u0447\u0435 \u043D\u0430\u0432\u0435\u0434\u0435\u043D\u043E \u043F\u0440\u043E\
  \u0441\u0442\u0438\u0439\u2026"
lastmod: '2024-03-13T22:44:49.602374-06:00'
model: gpt-4-0125-preview
summary: "\u0412 \u043E\u0441\u043D\u043E\u0432\u0456 \u0441\u0432\u043E\u0457\u0439\
  \ Bash \u0434\u043E\u0437\u0432\u043E\u043B\u044F\u0454 \u043F\u0435\u0440\u0435\
  \u0432\u0456\u0440\u044F\u0442\u0438 \u0456\u0441\u043D\u0443\u0432\u0430\u043D\u043D\
  \u044F \u0434\u0438\u0440\u0435\u043A\u0442\u043E\u0440\u0456\u0457, \u0432\u0438\
  \u043A\u043E\u0440\u0438\u0441\u0442\u043E\u0432\u0443\u044E\u0447\u0438 \u0443\u043C\
  \u043E\u0432\u043D\u0456 \u043E\u043F\u0435\u0440\u0430\u0442\u043E\u0440\u0438\
  \ \u0442\u0430 \u043E\u043F\u0435\u0440\u0430\u0442\u043E\u0440 `-d`."
title: "\u041F\u0435\u0440\u0435\u0432\u0456\u0440\u043A\u0430 \u043D\u0430\u044F\u0432\
  \u043D\u043E\u0441\u0442\u0456 \u0434\u0438\u0440\u0435\u043A\u0442\u043E\u0440\u0456\
  \u0457"
weight: 20
---

## Як це зробити:
В основі своїй Bash дозволяє перевіряти існування директорії, використовуючи умовні оператори та оператор `-d`. Нижче наведено простий приклад, який демонструє, як виконати цю перевірку.

```bash
if [ -d "/шлях/до/директорії" ]; then
    echo "Директорія існує."
else
    echo "Директорія не існує."
fi
```

Приклад виводу (якщо директорія існує):
```
Директорія існує.
```

Приклад виводу (якщо директорія не існує):
```
Директорія не існує.
```

Для більш складних скриптів поширеною практикою є поєднання перевірки з іншими операціями, наприклад створення директорії, якщо вона не існує:

```bash
DIR="/шлях/до/директорії"
if [ -d "$DIR" ]; then
    echo "$DIR існує."
else
    echo "$DIR не існує. Створюємо..."
    mkdir -p "$DIR"
    echo "$DIR створено."
fi
```

Приклад виводу (якщо директорія не існує, а потім створена):
```
/шлях/до/директорії не існує. Створюємо...
/шлях/до/директорії створено.
```

Хоча сам Bash надає надійні інструменти для таких перевірок, не існує популярних сторонніх бібліотек спеціально для цього завдання, оскільки вбудовані команди Bash повністю здатні та ефективні для перевірки наявності директорій.
