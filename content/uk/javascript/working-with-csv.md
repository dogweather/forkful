---
title:                "Робота з csv"
html_title:           "Javascript: Робота з csv"
simple_title:         "Робота з csv"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/javascript/working-with-csv.md"
---

{{< edit_this_page >}}

## Чому 

Що ж, ви вирішили попрацювати з CSV-файлами. Можливо, ви хочете зробити аналіз даних або зробити експорт даних з вашого сайту. І можливо, у вас є інші причини. Але головне, що ви прагнете дізнатися, як працювати з CSV-файлами за допомогою JavaScript.

## Як 

Найпростіший спосіб прочитати CSV-файл за допомогою JavaScript - це використовувати бібліотеку Papaparse. Для початку імпортуємо бібліотеку:

```JavaScript
import Papa from 'papaparse';
```

Далі ми можемо прочитати CSV-файл та отримати його дані у вигляді масиву об'єктів:

```JavaScript
Papa.parse('file.csv', {
    header: true,
    complete: function(result) {
        console.log(result.data);
    }
});
```

Тут ми використали опцію `header: true`, щоб першу рядок CSV-файлу використати як назви стовпців. Крім того, ми викликаємо функцію `complete`, яка отримує об'єкт результату з усіма даними з CSV-файлу.

Також ми можемо зберегти дані у вигляді CSV-файлу за допомогою такого коду:

```JavaScript
const data = [
    { id: 1, name: 'John', age: 25 },
    { id: 2, name: 'Jane', age: 30 },
    { id: 3, name: 'Bob', age: 40 }
];

const csv = Papa.unparse(data);
```

В результаті отримаємо такий вміст CSV-файлу:

```plain
"id","name","age"
1,"John",25
2,"Jane",30
3,"Bob",40
```

## Глибока розробка

Якщо вам потрібно більше контролю над обробкою CSV-файлів, то ви можете використовувати вбудований об'єкт `CSVReader`. Наприклад:

```JavaScript
const reader = new CSVReader();
const data = await reader.read('file.csv');
```

Тут ми створюємо об'єкт `CSVReader` і використовуємо його метод `read`, щоб прочитати CSV-файл та отримати дані у вигляді масиву.

Крім того, ви можете використовувати регулярні вирази для розбиття CSV-рядків та власні функції обробки даних. Недоліком цього підходу є більше коду, але ви отримуєте більше гнучкості у роботі з даними.

## Дивіться також

- [Papaparse documentation](https://www.papaparse.com/docs/)
- [JavaScript CSV tutorial](https://www.w3schools.com/js/js_json_csv.asp)
- [Parsing CSV files with Node.js](https://stackabuse.com/reading-and-writing-csv-files-with-node-js/)