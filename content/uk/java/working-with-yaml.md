---
title:                "Робота з YAML"
date:                  2024-01-19
html_title:           "Arduino: Робота з YAML"
simple_title:         "Робота з YAML"
programming_language: "Java"
category:             "Java"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/java/working-with-yaml.md"
---

{{< edit_this_page >}}

## What & Why?
Що таке робота з YAML і навіщо це роблять програмісти? YAML – це формат, що зручний для людини для серіалізації даних, який часто використовують для конфігурації. Програмісти використовують YAML, тому що він читабельний і інтуїтивно зрозумілий.

## How to:
```Java
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.dataformat.yaml.YAMLFactory;
import java.io.File;
import java.util.Map;

public class YamlExample {
    public static void main(String[] args) {
        try {
            ObjectMapper mapper = new ObjectMapper(new YAMLFactory());
            Map<String, Object> data = mapper.readValue(new File("config.yaml"), Map.class);
            
            System.out.println(data);
        } catch (Exception e) {
            e.printStackTrace();
        }
    }
}
```

Після запуску коду, якщо у файлі `config.yaml` є такий вміст:

```yaml
database:
  host: localhost
  port: 3306
```

Виведення буде таким:

```
{database={host=localhost, port=3306}}
```

## Deep Dive
YAML започатковано у 2001 році як більш читабельна альтернатива XML і JSON. Існують бібліотеки для роботи з YAML в більшості мов програмування. В Java, бібліотека Jackson з її YAML модулем – популярний вибір. Важливо пам'ятати про відступи в YAML, оскільки вони визначають структуру документа. 

## See Also
- YAML офіційний сайт: https://yaml.org/
- Jackson домашня сторінка: https://github.com/FasterXML/jackson
- YAML Wikipedia сторінка: https://uk.wikipedia.org/wiki/YAML
- Стандарт YAML 1.2: https://yaml.org/spec/1.2/spec.html
