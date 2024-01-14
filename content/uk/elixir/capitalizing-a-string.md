---
title:    "Elixir: Написання рядка з великої літери"
keywords: ["Elixir"]
---

{{< edit_this_page >}}

Що: Програмування на Elixir останнім часом стало все більш популярним серед українських розробників. Однією з корисних функцій цієї мови програмування є можливість капіталізувати рядки. Це важливо, насамперед, для збільшення читабельності виведених даних або для маніпулювання з рядками у програмі.

Як: Щоб скористатися функцією капіталізації рядка, потрібно використовувати метод "String.capitalize/1". Цей метод отримує один аргумент - рядок, який ми хочемо капіталізувати. Наприклад:

```
Elixir
iex> String.capitalize("hello")
"Hello"
```

У цьому прикладі ми капіталізуємо слово "hello" і отримуємо вивід "Hello". Проте, якщо ми застосуємо цей метод до вже капіталізованого рядка, нічого не відбудеться:

```
Elixir
iex> String.capitalize("Hello")
"Hello"
```

Це означає, що метод "String.capitalize/1" не модифікує вихідний рядок, а лише повертає новий капіталізований рядок. Також, варто зазначити, що цей метод працює лише з англійськими літерами, які мають заголовну та строчну форму. Інші символи залишаться без змін.

Глибоке погруження: Метод "String.capitalize/1" базується на функції "String.upcase/1", яка переводить кожну літеру в рядку у верхній регістр. Далі, використовуючи метод "String.downcase/1", перша літера рядка переводиться у нижній регістр, а решта залишаються в верхньому. Це дозволяє капіталізувати лише першу літеру в рядку, а решту залишити без змін.

See Also: [Документація Elixir](https://elixir-lang.org/docs.html), [Офіційний сайт Elixir](https://elixir-lang.org/), [Стаття "Навчання Elixir"](https://www.learnelixir.tv/blog/)