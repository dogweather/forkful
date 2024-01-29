---
title:                "Работа с YAML"
date:                  2024-01-29T00:05:31.735896-07:00
model:                 gpt-4-0125-preview
simple_title:         "Работа с YAML"
programming_language: "Java"
category:             "Java"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/java/working-with-yaml.md"
changelog:
  - 2024-01-29, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и почему?

YAML, "YAML не язык разметки", это язык сериализации данных. Программисты используют его из-за его читаемости и простоты, особенно для файлов конфигурации, первичных дампов данных или в качестве формата коммуникации между различными системами.

## Как это сделать:

Чтобы работать с YAML в Java, давайте использовать `snakeyaml`, популярную библиотеку.

Сначала добавьте зависимость в ваш `pom.xml`:

```xml
<dependency>
    <groupId>org.yaml</groupId>
    <artifactId>snakeyaml</artifactId>
    <version>1.29</version>
</dependency>
```

Теперь прочитаем файл YAML:

```java
import org.yaml.snakeyaml.Yaml;
import java.io.InputStream;
import java.util.Map;

public class YamlReader {
    public static void main(String[] args) {
        Yaml yaml = new Yaml();
        try (InputStream in = YamlReader.class
            .getClassLoader()
            .getResourceAsStream("config.yaml")) {
            
            Map<String, Object> data = yaml.load(in);
            System.out.println(data);
            
        } catch (Exception e) {
            e.printStackTrace();
        }
    }
}
```

Предположим, `config.yaml` выглядит так:

```yaml
version: '1.0'
services:
  webapp:
    build: .
    ports:
      - "5000:5000"
```

Вывод будет представлением `Map` вашего YAML:

```
{version=1.0, services={webapp={build=., ports=[5000:5000]}}}
```

Теперь давайте запишем YAML:

```java
import org.yaml.snakeyaml.Yaml;
import java.io.FileWriter;
import java.io.IOException;
import java.util.HashMap;
import java.util.Map;

public class YamlWriter {
    public static void main(String[] args) {
        Yaml yaml = new Yaml();
        Map<String, Object> data = new HashMap<>();
        
        data.put("name", "myapp");
        data.put("version", "2.0");
        
        try (FileWriter writer = new FileWriter("output.yaml")) {
            yaml.dump(data, writer);
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}
```

Проверьте `output.yaml`, чтобы увидеть новое содержимое YAML:

```yaml
name: myapp
version: '2.0'
```

## Подробнее

YAML появился в начале 2000-х как альтернатива XML для более простого структурирования данных. Несмотря на то что подъем JSON затмил его для коммуникации API, дружественность к человеку YAML делает его популярным для конфигураций. Те же данные, но JSON и TOML являются альтернативами YAML в зависимости от случаев использования. Одна оговорка YAML: для отступов разрешены только пробелы, табуляция не допускается.

## Смотрите также

Изучите дальше с помощью этих ресурсов:

- Официальная спецификация YAML: https://yaml.org/spec/1.2.2/
- Репозиторий snakeyaml на GitHub: https://github.com/asomov/snakeyaml
- YAML против JSON: https://phoenixnap.com/kb/yaml-vs-json
- YAML Lint, для проверки ваших файлов YAML: http://www.yamllint.com/
