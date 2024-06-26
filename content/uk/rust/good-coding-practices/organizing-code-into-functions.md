---
date: 2024-01-26 01:16:35.125885-07:00
description: "\u042F\u043A \u0446\u0435 \u0437\u0440\u043E\u0431\u0438\u0442\u0438\
  : \u041F\u0440\u0438\u043F\u0443\u0441\u0442\u0456\u043C\u043E, \u0443 \u0432\u0430\
  \u0441 \u0454 \u043A\u043E\u0434, \u0449\u043E \u043A\u0430\u043B\u044C\u043A\u0443\
  \u043B\u044E\u0454 \u043F\u043B\u043E\u0449\u0443 \u043A\u043E\u043B\u0430 \u043A\
  \u0456\u043B\u044C\u043A\u0430 \u0440\u0430\u0437\u0456\u0432. \u0417\u0430\u043C\
  \u0456\u0441\u0442\u044C \u043F\u043E\u0432\u0442\u043E\u0440\u0435\u043D\u043D\u044F\
  \ \u0444\u043E\u0440\u043C\u0443\u043B\u0438, \u0432\u0438 \u043E\u0431\u0435\u0440\
  \u0442\u0430\u0454\u0442\u0435 \u0457\u0457 \u0443 \u0444\u0443\u043D\u043A\u0446\
  \u0456\u044E."
lastmod: '2024-03-13T22:44:48.945811-06:00'
model: gpt-4-0125-preview
summary: "\u041F\u0440\u0438\u043F\u0443\u0441\u0442\u0456\u043C\u043E, \u0443 \u0432\
  \u0430\u0441 \u0454 \u043A\u043E\u0434, \u0449\u043E \u043A\u0430\u043B\u044C\u043A\
  \u0443\u043B\u044E\u0454 \u043F\u043B\u043E\u0449\u0443 \u043A\u043E\u043B\u0430\
  \ \u043A\u0456\u043B\u044C\u043A\u0430 \u0440\u0430\u0437\u0456\u0432."
title: "\u041E\u0440\u0433\u0430\u043D\u0456\u0437\u0430\u0446\u0456\u044F \u043A\u043E\
  \u0434\u0443 \u0432 \u0444\u0443\u043D\u043A\u0446\u0456\u0457"
weight: 18
---

## Як це зробити:
Припустімо, у вас є код, що калькулює площу кола кілька разів. Замість повторення формули, ви обертаєте її у функцію.

```Rust
fn calculate_circle_area(radius: f64) -> f64 {
    std::f64::consts::PI * radius.powi(2)
}

fn main() {
    let radius = 5.0;
    let area = calculate_circle_area(radius);
    println!("Площа кола становить: {}", area);
}
```

Вивід:

```
Площа кола становить: 78.53981633974483
```

## Поглиблений аналіз
Історично функції прийшли з математики, де вони відображають вхідні дані на вихідні. У програмуванні вони існують із часів асемблеру, хоча ми називали їх "підпрограмами". Функції в Rust можуть повертати значення та навіть інші функції завдяки функціям першого класу та замиканням.

Альтернативи? Вбудований код або макроси, але вони заплутані для складної логіки. Об'єкти з методами - ще один спосіб організації функціональності, інший смак, ніж стандартні функції.

Імплементація в Rust досить проста. Функції оголошують типи своїх параметрів і тип повернення. За звичкою вони мають імена у стилі 'snake case'. Є публічні функції (`pub fn`) для використання поза модулем та приватні для внутрішнього використання. І в Rust є ця класна функція, коли вам не потрібно ключове слово `return` для останнього виразу в функції.

## Дивіться також
Перегляньте це для додаткової інформації:
- Книга "Мова програмування Rust": [Функції](https://doc.rust-lang.org/book/ch03-03-how-functions-work.html)
- Rust на прикладах про [Функції](https://doc.rust-lang.org/rust-by-example/fn.html)
