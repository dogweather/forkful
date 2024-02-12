---
title:                "Робота з CSV"
aliases:
- /uk/bash/working-with-csv.md
date:                  2024-02-03T19:18:50.296588-07:00
model:                 gpt-4-0125-preview
simple_title:         "Робота з CSV"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/bash/working-with-csv.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Що та чому?
Робота з файлами CSV (значення, розділені комами) в Bash полягає у обробці та маніпуляції табличними даними, збереженими у форматі простого тексту. Це необхідно для програмістів, оскільки це дозволяє автоматизувати завдання трансформації, аналізу та інтеграції даних безпосередньо з командного рядка, без необхідності використання більш важких інструментів або програмних середовищ.

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
