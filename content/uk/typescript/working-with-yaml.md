---
title:                "Робота з yaml"
html_title:           "TypeScript: Робота з yaml"
simple_title:         "Робота з yaml"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/typescript/working-with-yaml.md"
---

{{< edit_this_page >}}

Що і чому?
Робота з YAML - це процес, що дозволяє програмістам працювати з структурованими даними у форматі YAML (YAML Ain't Markup Language). Цей формат дуже зручний для зберігання та обробки різноманітних даних, таких як налаштування програм, конфігураційні файли та багато чого іншого.

Як це зробити:
```TypeScript
import * as yaml from 'js-yaml';

// Write YAML data
const data = { name: 'John Doe', age: 30 };
const yamlData = yaml.safeDump(data);

console.log(yamlData);

// Output:
// name: John Doe
// age: 30

// Read YAML data
const parsedData = yaml.safeLoad(yamlData);

console.log(parsedData.name);
console.log(parsedData.age);

// Output:
// John Doe
// 30
```

Поглиблене дослідження:
- Історичний контекст: YAML був створений для того, щоб бути простим та легким у використанні форматом обміну даними. Вперше цей формат був випущений в 2001 році та відтоді став дуже популярним у світі програмування.
- Альтернативи: існують інші формати для структурованих даних, такі як JSON та XML, проте YAML часто використовується завдяки своїй зручності та читабельності.
- Деталі реалізації: у TypeScript для роботи з YAML зазвичай використовують бібліотеку `js-yaml`, яка надає зручні функції для обробки та конвертації даних у форматі YAML.

Дивіться також:
- Офіційна документація по бібліотеці `js-yaml`: https://nodeca.github.io/js-yaml/
- Детальніше про формат YAML: https://yaml.org/