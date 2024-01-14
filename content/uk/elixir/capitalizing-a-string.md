---
title:    "Elixir: Перетворення тексту у верхній регістр"
keywords: ["Elixir"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/uk/elixir/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Чому

Під час програмування у Еліксирі часто зустрічається необхідність використовувати рядки. Деякі функції вимагають, щоб рядки були написані з великої букви. Наприклад, при роботі з іменами користувачів або назвами файлів. У цьому випадку, функція capitalize може бути дуже корисною. Вона перетворює першу букву рядка на велику.

## Як використовувати

Для використання capitalize потрібно викликати його як метод модуля String. Наприклад:

```Elixir
String.capitalize("еліксир") #=> "Еліксир"
```

Якщо у рядку вже є велика буква, то capitalize залишить її такою.

```Elixir
String.capitalize("Elixir") #=> "Elixir"
```

Також capitalize може бути використаний для перетворення всього рядка на великі літери, за допомогою методу capitalize/1.

```Elixir
String.capitalize("еліксир", :all) #=> "ЕЛІКСИР"
```

## Поглиблення

Функція capitalize фактично використовує метод String.upcase/1, який перетворює всі символи рядка на великі літери. Але перед тим, як виконувати upcase, capitalize перевіряє, чи є перший символ рядка великою літерою, і якщо так, то просто повертає вхідний рядок без змін. Якщо ж перший символ не є великою літерою, то capitalize виконує String.upcase, а потім повертає отриманий результат з першою великою літерою.

Це простий приклад імплементації capitalize у Еліксирі:

```Elixir
def capitalize(string) do
  if String.upcase(String.slice(string, 0)) == String.slice(string, 0) do
    string
  else
    String.upcase(string)
  end
end
```

Цей приклад також показує використання методів String.slice та String.upcase.

## Дивись також:

- [Документація String.capitalize](https://hexdocs.pm/elixir/String.html#capitalize/1)
- [Документація String.upcase](https://hexdocs.pm/elixir/String.html#upcase/1)
- [Документація String.slice](https://hexdocs.pm/elixir/String.html#slice/3)