---
title:                "Робота з yaml"
html_title:           "Javascript: Робота з yaml"
simple_title:         "Робота з yaml"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/javascript/working-with-yaml.md"
---

{{< edit_this_page >}}

Ой-ой, алгоритмами і двійними гранами наповнені тіші YAML-файли, як зрозуміти їхню мову?! 
## Що і чому?
YAML - це простий формат файлів, який використовують програмісти для збереження та передачі данних. Він створений для того, щоб зробити читання та запис даних більш зручним та легким для розуміння. Так, з YAML ми можемо швидко і зенітно передати наші дані між різними системами.
## Як:
```Javascript
let data = `
name: Jane
age: 25
hobbies:
  - hiking
  - reading
  - cooking
`;
let obj = jsyaml.safeLoad(data);
console.log(obj.name) // Jane
console.log(obj.age) // 25
console.log(obj.hobbies) // ["hiking", "reading", "cooking"]
```
## Глибочинний розгляд: 
1. Історичний контекст: YAML був створений в 2001 році як альтернатива формату XML. Його назва походить від абревіатури "YAML Ain't a Markup Language" (YAML - це не мова розмітки). 
2. Альтернативи: існують різні формати для збереження та передачі даних, такі як JSON, XML та CSV. Вибір формату залежить від конкретної потреби та комфорту користувача.
3. Деталі реалізації: YAML зазвичай використовується для конфігурацій файлів, включаючи Docker-compose та Kubernetes. Він також може бути використаний для збереження маршрутів та даних у веб-додатках.
## Дивись також:
1. [Офіційна сторінка YAML](https://yaml.org/)
2. [Документація по Javascript для роботи з YAML](https://www.npmjs.com/package/js-yaml)
3. [Порівняння YAML та інших форматів даних](https://stackify.com/yaml-vs-json-xml/)