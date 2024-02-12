---
title:                "Робота з CSV"
aliases:
- /uk/google-apps-script/working-with-csv.md
date:                  2024-02-01T22:06:12.913538-07:00
model:                 gpt-4-0125-preview
simple_title:         "Робота з CSV"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/google-apps-script/working-with-csv.md"
changelog:
  - 2024-02-01, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Що та чому?

Робота з файлами CSV (Comma-Separated Values, значення, що розділені комами) у Google Apps Script передбачає читання, модифікацію та запис звичайних текстових файлів, де кожен рядок представляє запис даних, значення в якому розділені комами. Програмісти роблять це для легкого обміну даними між різними додатками, базами даних або мовами програмування завдяки широкому прийняттю CSV як простого текстового формату обміну даними.

## Як:

### Читання даних CSV

Щоб прочитати дані CSV з файлу, збереженого в Google Drive, спочатку потрібно отримати вміст файлу як рядок, а потім розібрати його. Google Apps Script робить отримання вмісту файлу простим за допомогою сервісу DriveApp.

```javascript
function readCSV() {
  var fileId = 'YOUR_FILE_ID_HERE'; // Замініть на актуальний ідентифікатор файлу
  var file = DriveApp.getFileById(fileId);
  var content = file.getBlob().getDataAsString();
  var rows = content.split("\n");
  
  for (var i = 0; i < rows.length; i++) {
    var cells = rows[i].split(",");
    Logger.log(cells); // Реєстрація комірок кожного рядка
  }
}
```

### Запис даних CSV

Створення та запис у CSV передбачає конструювання рядка з комами-роздільниками значень та переводами рядка, а потім збереження або експорт його. Цей приклад демонструє створення нового файлу CSV у Google Drive.

```javascript
function writeCSV() {
  var folderId = 'YOUR_FOLDER_ID_HERE'; // Замініть на ідентифікатор папки Drive, де буде створено новий файл
  var csvContent = "Name,Age,Occupation\nJohn Doe,29,Engineer\nJane Smith,34,Designer";
  var fileName = "example.csv";
  
  var folder = DriveApp.getFolderById(folderId);
  folder.createFile(fileName, csvContent, MimeType.PLAIN_TEXT);
}
```

### Приклад виводу

При реєстрації комірок рядків при читанні CSV:

```plaintext
[John, 29, Engineer]
[Jane, 34, Designer]
```

При записі створюється файл з назвою "example.csv" і вмістом:

```plaintext
Name,Age,Occupation
John Doe,29,Engineer
Jane Smith,34,Designer
```

## Поглиблено

Історично, файли CSV користувалися популярністю через їхню простоту та читабельність для людей, що робило їх доступними для не-програмістів і корисними для завдань швидкого огляду даних. Однак, Google Apps Script діє в межах екосистеми Google, де Google Sheets служить потужною, зручною для користувача альтернативою для маніпуляцій з CSV. Sheets не лише надають графічний інтерфейс для редагування даних, але й підтримують складні формули, стилізацію та багато інших функцій, яких не має сирих CSV.

Попри переваги, які надає Google Sheets, пряма маніпуляція з файлами CSV у Google Apps Script залишається важливою для автоматизованих завдань, зокрема коли йдеться про роботу з зовнішніми системами, що генерують або потребують дані у форматі CSV. Наприклад, інтеграція зі старими системами, експорт даних для використання в інших додатках або попередня обробка перед поданням даних в Google Sheets.

Більше того, можливість Google Apps Script працювати з файлами CSV може бути розширена за допомогою сервісу Utilities для потреб у складному кодуванні або взаємодії з зовнішніми API для завдань конвертації, розбору або перевірки. Однак, для роботи з великими наборами даних або потребі у складних маніпуляціях, розгляньте можливість використання Google Sheets API або дослідження BigQuery для більш потужних можливостей обробки даних.

У той час як простота залишається ключовою причиною популярності CSV, ці альтернативи пропонують багатший набір можливостей для роботи з даними в широкій екосистемі Google Cloud.
