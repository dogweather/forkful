---
changelog:
- 2024-01-30, gpt-4-0125-preview, translated from English
date: 2024-01-30 19:12:32.593775-07:00
description: "\u0410\u0441\u043E\u0446\u0456\u0430\u0442\u0438\u0432\u043D\u0456 \u043C\
  \u0430\u0441\u0438\u0432\u0438 \u0432 Lua \u2013 \u0446\u0435 \u043D\u0456\u0431\
  \u0438 \u0441\u0435\u043A\u0440\u0435\u0442\u043D\u0456 \u0440\u0443\u043A\u043E\
  \u0441\u0442\u0438\u0441\u043A\u0430\u043D\u043D\u044F \u0434\u043B\u044F \u0434\
  \u0430\u043D\u0438\u0445\u2014\u0437\u0430\u043C\u0456\u0441\u0442\u044C \u0442\u043E\
  \u0433\u043E, \u0449\u043E\u0431 \u043F\u0440\u043E\u0441\u0442\u043E \u0447\u0438\
  \u0441\u043B\u0430 \u0441\u0442\u043E\u044F\u043B\u0438 \u0441\u043B\u0443\u0445\
  \u043D\u044F\u043D\u043E \u0432 \u0440\u044F\u0434\u043A\u0443 \u0437\u0430 \u0456\
  \u043D\u0434\u0435\u043A\u0441\u043E\u043C, \u0432\u0430\u0448\u0456 \u043A\u043B\
  \u044E\u0447\u0456 \u043C\u043E\u0436\u0443\u0442\u044C\u2026"
lastmod: '2024-03-13T22:44:49.487870-06:00'
model: gpt-4-0125-preview
summary: "\u0410\u0441\u043E\u0446\u0456\u0430\u0442\u0438\u0432\u043D\u0456 \u043C\
  \u0430\u0441\u0438\u0432\u0438 \u0432 Lua \u2013 \u0446\u0435 \u043D\u0456\u0431\
  \u0438 \u0441\u0435\u043A\u0440\u0435\u0442\u043D\u0456 \u0440\u0443\u043A\u043E\
  \u0441\u0442\u0438\u0441\u043A\u0430\u043D\u043D\u044F \u0434\u043B\u044F \u0434\
  \u0430\u043D\u0438\u0445\u2014\u0437\u0430\u043C\u0456\u0441\u0442\u044C \u0442\u043E\
  \u0433\u043E, \u0449\u043E\u0431 \u043F\u0440\u043E\u0441\u0442\u043E \u0447\u0438\
  \u0441\u043B\u0430 \u0441\u0442\u043E\u044F\u043B\u0438 \u0441\u043B\u0443\u0445\
  \u043D\u044F\u043D\u043E \u0432 \u0440\u044F\u0434\u043A\u0443 \u0437\u0430 \u0456\
  \u043D\u0434\u0435\u043A\u0441\u043E\u043C, \u0432\u0430\u0448\u0456 \u043A\u043B\
  \u044E\u0447\u0456 \u043C\u043E\u0436\u0443\u0442\u044C\u2026"
title: "\u0412\u0438\u043A\u043E\u0440\u0438\u0441\u0442\u0430\u043D\u043D\u044F \u0430\
  \u0441\u043E\u0446\u0456\u0430\u0442\u0438\u0432\u043D\u0438\u0445 \u043C\u0430\u0441\
  \u0438\u0432\u0456\u0432"
---

{{< edit_this_page >}}

## Що та Чому?

Асоціативні масиви в Lua – це ніби секретні рукостискання для даних—замість того, щоб просто числа стояли слухняно в рядку за індексом, ваші ключі можуть бути чим завгодно, що робить отримання даних дуже зручним. Чому програмісти їх використовують? Тому що іноді потрібно звертатися до шматочка даних за його ім'ям, а не за номером в ряді.

## Як це зробити:

В Lua створення асоціативного масиву (або таблиці, як кажуть у Lua) є досить простим. Ви забуваєте про звичні числові індекси на користь ключів на ваш вибір. Подивіться на це:

```Lua
-- Створення асоціативного масиву
userInfo = {
  name = "Jamie",
  occupation = "Мандрівник",
  level = 42
}

-- Доступ до елементів
print(userInfo["name"]) -- Виводить Jamie
print(userInfo.occupation) -- Виводить Мандрівник

-- Додавання нових пар ключ-значення
userInfo["hobby"] = "Програмування"
userInfo.favLang = "Lua"

-- Ітерація через асоціативний масив
for key, value in pairs(userInfo) do
  print(key .. ": " .. value)
end
```

Вивід:
```
Jamie
Мандрівник
name: Jamie
occupation: Мандрівник
level: 42
hobby: Програмування
favLang: Lua
```

Частина, що охоплює? Ви взаємодієте з даними, використовуючи ключі, значущі для вас, що робить код більш зрозумілим і легшим для підтримки.

## Поглиблений Розгляд

Коли Lua з'явилася на сцені, вона представила таблиці як універсальну структуру даних, революціонізувавши те, як розробники керують даними. На відміну від деяких мов, де асоціативні масиви та масиви є окремими сутностями, таблиці Lua слугують і тим, і іншим, спрощуючи ландшафт структур даних.

Те, що робить таблиці Lua особливо потужними, - це їх гнучкість. Однак, ця гнучкість має свою ціну у вигляді потенційного впливу на продуктивність, особливо з великими наборами даних, де більш спеціалізована структура даних може бути переважною для ефективності.

Хоча Lua з коробки не підтримує більш традиційні структури даних, такі як зв'язні списки або хеш-таблиці, адаптивність структури таблиці означає, що ви можете реалізувати ці структури за допомогою таблиць, якщо це потрібно. Просто пам'ятайте: з великою силою приходить велика відповідальність. Розумно використовуйте гнучкість, щоб зберегти продуктивність та зрозумілість вашого коду.
