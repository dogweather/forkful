---
title:                "C: Робота з ямл."
simple_title:         "Робота з ямл."
programming_language: "C"
category:             "C"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/c/working-with-yaml.md"
---

{{< edit_this_page >}}

## Чому

ЯМЛ (YAML) - це формат даних, який дуже корисний для програмного забезпечення. Він дозволяє створювати читабельні і легкі для розуміння файли з даними, що робить їх ідеальними для збереження налаштувань та інших конфігураційних даних.

## Як створити

Якщо ви вже знаєте мову С, ви можете легко почати використовувати ЯМЛ у своїх програмах. Потрібно лише підключити бібліотеку yaml.h та використовувати функції з неї. Ось приклад, як це можна зробити:

```C
#include <yaml.h>

int main() {
    // Створюємо структуру для зберігання даних
    yaml_document_t doc;
    yaml_document_initialize(&doc, NULL, NULL, NULL, 0, 0); 

    // Створюємо мінімальну структуру документу
    yaml_node_t *root = yaml_document_add_mapping(&doc, NULL, YAML_BLOCK_MAPPING_STYLE);

    // Додаємо пару ключ-значення
    yaml_document_append_mapping_pair(&doc, root, 
        yaml_document_add_scalar(&doc, NULL, (yaml_char_t *)"name", 0, YAML_PLAIN_SCALAR_STYLE),
        yaml_document_add_scalar(&doc, NULL, (yaml_char_t *)"John Smith", 0, YAML_PLAIN_SCALAR_STYLE));

    // Виводимо структуру документу у файл
    FILE *output = fopen("output.yaml", "w");
    yaml_document_emit(&doc, output);
    fclose(output);

    // Звільняємо пам’ять
    yaml_document_delete(&doc);
    yaml_document_initialize(&doc, NULL, NULL, NULL, 0, 0); 

    return 0;
}
```

В результаті ви отримаєте файл з таким вмістом:

```yaml
name: John Smith
```

## Глибока погрузка

ЯМЛ має багато особливостей, які можуть бути корисними для програмістів. Наприклад, ви можете використовувати ЯМЛ для збереження багаторівневих структур даних. Також, ви можете використовувати різні стилі форматування, щоб зробити ваші файли з даними ще більш зрозумілими і зручними для читання. Для детального поглибленого вивчення можна ознайомитися з офіційною документацією бібліотеки [libyaml](https://yaml.org/spec/1.2/spec.html#), яка підтримує роботу з ЯМЛ у мові С.

## Дивіться також

- [Офіційний веб-сайт ЯМЛ](https://yaml.org/)
- [Огляд ЯМЛ у мові програмування С](https://medium.com/programming-huddle/introduction-to-yaml-c-libyaml-api-codes-implementation-8031c65c0b80)
- [Основи роботи з ЯМЛ у Python](https://www.datacamp.com/community/tutorials/understanding-yaml-python)