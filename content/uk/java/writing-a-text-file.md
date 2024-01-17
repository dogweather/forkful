---
title:                "Створення текстового файлу"
html_title:           "Java: Створення текстового файлу"
simple_title:         "Створення текстового файлу"
programming_language: "Java"
category:             "Java"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/java/writing-a-text-file.md"
---

{{< edit_this_page >}}

Що & Чому?:
Написання текстового файлу - це процес створення і записування вмісту у файл. Програмісти роблять це для зберігання даних або виведення результатів своєї програми.

Як це зробити:
Програмування на Java має вбудований спосіб для написання текстового файлу - клас FileWriter. Для створення файлу ми використовуємо конструктор FileWriter, передаючи йому шлях до файлу та параметр, що вказує, чи необхідно додавати нові дані до вже існуючого файлу. Після цього ми можемо записати наші дані в файл за допомогою методу write () і закрити файл методом close ().

```java
FileWriter file = new FileWriter("file.txt", false);  
// Встановлюємо значення false, щоб перезаписати вміст файлу
file.write("Привіт, світ!");  
file.close();
```

Глибше вивчення:
Написання текстового файлу не є новим поняттям - раніше використовувалися більш старі методи, такі як FileOutputStream або PrintWriter. Однак, клас FileWriter є більш зручним і має більше можливостей для роботи з файлами тексту. Також, існують альтернативні методи для запису даних в файл, такі як використання буферизації з більш високою продуктивністю.

Посилання:
- [Документація Java для класу FileWriter] (https://docs.oracle.com/javase/10/docs/api/java/io/FileWriter.html)
- [Стаття на Medium про роботу з файлами в Java] (https://medium.com/@javinpaul/java-8-stream-api-example-tutorial-from-developer-3d23f97e197a)
- [Стаття на Habr про роботу з файлами в Java] (https://habr.com/ru/post/438582/)