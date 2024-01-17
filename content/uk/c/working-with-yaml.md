---
title:                "Робота з форматом yaml"
html_title:           "C: Робота з форматом yaml"
simple_title:         "Робота з форматом yaml"
programming_language: "C"
category:             "C"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/c/working-with-yaml.md"
---

{{< edit_this_page >}}

Що & Чому?
Робота з YAML - це простий спосіб для організації та збереження данних у форматі ключ-значення. Це дозволяє програмістам легко зчитувати та зберігати конфігураційні файли та дані для їх програм. 

Як?
Нижче показано код C, який використовує бібліотеку LibYAML для зчитування і запису даних у форматі YAML. Також показані приклади вхідних та вихідних даних для кращого розуміння. 

```C
#include <yaml.h>

// Структура даних для збереження YAML документу
typedef struct {
    char key[50];
    char value[50];
} yaml_data;

// Функція для обробки YAML документу
int process_yaml(yaml_parser_t *parser, yaml_data *data) {

    yaml_event_t event;
    int done = 0;

    // Постійно отримуємо нові події, поки не дійдемо до кінця документу
    while(!done) {
        // Отримуємо наступну подію та перевіряємо, чи не виникли помилки
        if(!yaml_parser_parse(parser, &event)) {
            printf("Помилка при обробці події\n");
            return 0;
        }
        // Реагуємо на різні типи подій
        switch(event.type) {
            // Початок документу
            case YAML_DOCUMENT_START_EVENT:
                // Реалізуємо якщо потрібно
                break;
            // Кінець документу
            case YAML_DOCUMENT_END_EVENT:
                done = 1; // Завершуємо обробку, якщо досягли кінця
                break;
            // Початок мапи ключ-значення
            case YAML_MAPPING_START_EVENT:
                // Реалізуємо якщо потрібно
                break;
            // Кінець мапи ключ-значення
            case YAML_MAPPING_END_EVENT:
                // Реалізуємо якщо потрібно
                break;
            // Початок нового елементу
            case YAML_SCALAR_EVENT:
                // Зчитуємо ключ та значення
                if(!strcmp(event.data.scalar. tag, "key")) {
                    strcpy(data->key, event.data.scalar.value);
                }
                if(!strcmp(event.data.scalar.tag, "value")) {
                    strcpy(data->value, event.data.scalar.value);
                }
                break;
            // Інші типи подій, які можуть бути ігноровані
            default:
                break;
        }
        // Проводимо очищення після обробки події
        yaml_event_delete(&event);
    }

    return 1;
}

``` 

Приклад вхідного YAML документу:

``` 
key: "value"
language: "C"
```

Виведення ключа та значення:

``` 
key: "value"
language: "C"
```

Глибокий занурення:
Історичний контекст:
YAML була створена в 2001 році як формат для збереження даних, придатний для людського читання та підключення до різних мов програмування. Хоча спочатку використовувалася переважно для мови Ruby, зараз вона підтримується більшістю мов програмування.

Альтернативи:
YAML не є єдиним форматом для збереження даних. Інші популярні формати включають JSON та XML. Якщо вам потрібна більша структурованість та можливість зберігати багатовимірні дані, то JSON може бути кращим вибором, але для простих конфігураційних даних YAML є більш зручним та простим у використанні.

Деталі реалізації:
Реалізація обробки YAML документу полягає у використанні бібліотеки LibYAML, яка надає потужні інструменти для роботи з YAML в C. Ця бібліотека є кросплатформеною та надає можливості для зчитування та запису YAML документу, а також підтримує безліч різних типів даних.

Див. також:
- Офіційна документація LibYAML: [https://pyyaml.org/wiki/LibYAML](https://pyyaml.org/wiki/LibYAML)
- Стандарт YAML: [https://yaml.org/spec/1.2/spec.html](https://yaml.org/spec/1.2/spec.html)