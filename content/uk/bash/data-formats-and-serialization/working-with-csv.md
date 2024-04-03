---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:18:50.296588-07:00
description: "\u042F\u043A: **\u0427\u0438\u0442\u0430\u043D\u043D\u044F \u0444\u0430\
  \u0439\u043B\u0443 CSV \u0440\u044F\u0434\u043E\u043A \u0437\u0430 \u0440\u044F\u0434\
  \u043A\u043E\u043C**."
lastmod: '2024-03-13T22:44:49.616007-06:00'
model: gpt-4-0125-preview
summary: "**\u0427\u0438\u0442\u0430\u043D\u043D\u044F \u0444\u0430\u0439\u043B\u0443\
  \ CSV \u0440\u044F\u0434\u043E\u043A \u0437\u0430 \u0440\u044F\u0434\u043A\u043E\
  \u043C**."
title: "\u0420\u043E\u0431\u043E\u0442\u0430 \u0437 CSV"
weight: 37
---

## Як:
**Читання файлу CSV рядок за рядком**

```bash
while IFS=, read -r column1 column2 column3
do
  echo "Стовпчик 1: $column1, Стовпчик 2: $column2, Стовпчик 3: $column3"
done < sample.csv
```

*Приклад виводу:*

```
Стовпчик 1: id, Стовпчик 2: name, Стовпчик 3: email
...
```

**Фільтрація рядків CSV за умовою**

Використовуючи `awk`, ви можете легко фільтрувати рядки. Наприклад, щоб знайти рядки, де другий стовпчик дорівнює "Alice":

```bash
awk -F, '$2 == "Alice" { print $0 }' sample.csv
```

**Зміна значення стовпчика**

Щоб змінити другий стовпчик на великі літери:

```bash
awk -F, 'BEGIN {OFS=",";} { $2 = toupper($2); print $0; }' sample.csv
```

**Сортування файлу CSV за стовпчиком**

Ви можете сортувати файл CSV, наприклад, за третім стовпчиком (числово):

```bash
sort -t, -k3,3n sample.csv
```

**Використання `csvkit` для складніших завдань**

`csvkit` - це набір інструментів командного рядка для конвертації в CSV та роботи з ним. Його можна встановити через pip.

Для конвертування файлу JSON в CSV:

```bash
in2csv data.json > data.csv
```

Для запиту до файлу CSV за допомогою SQL:

```bash
csvsql --query "SELECT name FROM sample WHERE id = 10" sample.csv
```

*Примітка: Встановлення `csvkit` вимагає Python і може бути виконане за допомогою `pip install csvkit`.*
