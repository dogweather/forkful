---
title:                "Читання текстового файлу"
html_title:           "Java: Читання текстового файлу"
simple_title:         "Читання текстового файлу"
programming_language: "Java"
category:             "Java"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/java/reading-a-text-file.md"
---

{{< edit_this_page >}}

Що та чому?
Читання текстового файлу - це процес отримання інформації з файлу, який містить текстову інформацію. Програмісти роблять це для отримання даних із зовнішніх джерел, таких як бази даних або інші програми.

Як це зробити?
```Java
public static void main(String[] args) throws FileNotFoundException {
   File file = new File("myfile.txt");
   Scanner scanner = new Scanner(file);
   while (scanner.hasNextLine()){
       String line = scanner.nextLine();
       System.out.println(line);
   }
   scanner.close();
}
```
Використовуючи клас Scanner у Java, ми можемо створити об'єкт файлу та сканувати його рядок за рядком за допомогою методу `nextLine()`. У цьому прикладі ми друкуємо кожен рядок на екрані. Не забудьте закрити сканер після використання.

Глибоке погруження
В минулому, до появи класу Scanner, програмісти використовували клас BufferedReader для читання текстових файлів. Крім того, існують інші альтернативи, такі як використання потоків або регулярних виразів. Щоб точно визначити, яка опція найкраща для вашої програми, рекомендується зробити дослідження та провести власні тести.

Дивись також
1. Документація по класу Scanner: https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/util/Scanner.html
2. Стаття "Робота з текстовими файлами в Java": https://www.baeldung.com/java-read-lines-large-file