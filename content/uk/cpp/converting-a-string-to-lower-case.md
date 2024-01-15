---
title:                "Перетворення рядка в нижній регістр"
html_title:           "C++: Перетворення рядка в нижній регістр"
simple_title:         "Перетворення рядка в нижній регістр"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/cpp/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Чому
Написано на C++ програмування, багато разів необхідно змінювати регістр символів у тексті. Це може бути важливо для забезпечення правильності порівняння тексту або передачі даних.

## Як
```C++
string str = "Привіт Світ!"
transform(str.begin(), str.end(), str.begin(), ::tolower);
cout << str;
```
Результат: "привіт світ!"

## Deep Dive
У C++, зміна регістру символів у текстовому рядку може бути здійснена за допомогою функції `transform()`. Ця функція приймає два ітератори, які вказують на початок та кінець діапазону символів у текстовому рядку, а також функцію, яка буде застосовуватися до кожного символу у цьому діапазоні. У нашому випадку, ми використовуємо функцію `::tolower`, яка конвертує кожен символ у строчній регістр. Важливо враховувати, що ця функція не змінює сам текстовий рядок, тому потрібно зберегти його у змінну `str` після виклику `transform()`.
<br/>

## Дивіться також
- [C++ transform() documentation](https://en.cppreference.com/w/cpp/algorithm/transform)
- [Програмування на C++ в 2021: що варто знати початківцю](https://prog.kiev.ua/blog/programuvannia-na-c-v-2021.html)
- [C++ початковий курс від Освітньої платформи "Prometheus"](https://courses.prometheus.org.ua/courses/KPI/Programming101/2015_T1/about)