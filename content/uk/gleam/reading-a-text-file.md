---
title:                "Gleam: Читання текстового файлу"
simple_title:         "Читання текстового файлу"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/gleam/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Чому
Вам може бути цікаво прочитати тектовий файл, коли підготовка до програмування мовою Gleam. Є багато корисної інформації, яка зберігається в текстових файлах і може підказати вам, як писати більш ефективний код.

## Як
```Gleam
f <- File.open("myfile.txt")
text <- File.read(f)
File.close(f)
println(text)
```

Цей код відкриває файл "myfile.txt" і зберігає його в змінну "f". Наступним кроком є читання файлу за допомогою "File.read", після чого файл закривається функцією "File.close". Нарешті, змінну "text" друкується, щоб на екрані вивести вміст файлу.

## Deep Dive
Подальший розгляд роботи з текстовими файлами у Gleam може включати в себе роботу з різними розділами файлу, додавання, видалення і редагування вмісту. Також важливо зберігати файл з правильною кодуванням, щоб уникнути проблем з читанням файлу.

## See Also
- [Gleam Documentation](https://gleam.run/documentation/)
- [Reading and Writing Files in Gleam](https://gleam.run/documentation/examples/files.html)
- [Working with Files in Gleam](https://medium.com/@jjst/beginners-guide-to-using-files-in-gleam-e77614dd1d21)