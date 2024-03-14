---
changelog:
- 2024-01-29, gpt-4-0125-preview, translated from English
date: 2024-01-29 00:05:31.735896-07:00
description: "YAML, \"YAML \u043D\u0435 \u044F\u0437\u044B\u043A \u0440\u0430\u0437\
  \u043C\u0435\u0442\u043A\u0438\", \u044D\u0442\u043E \u044F\u0437\u044B\u043A \u0441\
  \u0435\u0440\u0438\u0430\u043B\u0438\u0437\u0430\u0446\u0438\u0438 \u0434\u0430\u043D\
  \u043D\u044B\u0445. \u041F\u0440\u043E\u0433\u0440\u0430\u043C\u043C\u0438\u0441\
  \u0442\u044B \u0438\u0441\u043F\u043E\u043B\u044C\u0437\u0443\u044E\u0442 \u0435\
  \u0433\u043E \u0438\u0437-\u0437\u0430 \u0435\u0433\u043E \u0447\u0438\u0442\u0430\
  \u0435\u043C\u043E\u0441\u0442\u0438 \u0438 \u043F\u0440\u043E\u0441\u0442\u043E\
  \u0442\u044B, \u043E\u0441\u043E\u0431\u0435\u043D\u043D\u043E \u0434\u043B\u044F\
  \ \u0444\u0430\u0439\u043B\u043E\u0432\u2026"
lastmod: '2024-03-13T22:44:44.860421-06:00'
model: gpt-4-0125-preview
summary: "YAML, \"YAML \u043D\u0435 \u044F\u0437\u044B\u043A \u0440\u0430\u0437\u043C\
  \u0435\u0442\u043A\u0438\", \u044D\u0442\u043E \u044F\u0437\u044B\u043A \u0441\u0435\
  \u0440\u0438\u0430\u043B\u0438\u0437\u0430\u0446\u0438\u0438 \u0434\u0430\u043D\u043D\
  \u044B\u0445. \u041F\u0440\u043E\u0433\u0440\u0430\u043C\u043C\u0438\u0441\u0442\
  \u044B \u0438\u0441\u043F\u043E\u043B\u044C\u0437\u0443\u044E\u0442 \u0435\u0433\
  \u043E \u0438\u0437-\u0437\u0430 \u0435\u0433\u043E \u0447\u0438\u0442\u0430\u0435\
  \u043C\u043E\u0441\u0442\u0438 \u0438 \u043F\u0440\u043E\u0441\u0442\u043E\u0442\
  \u044B, \u043E\u0441\u043E\u0431\u0435\u043D\u043D\u043E \u0434\u043B\u044F \u0444\
  \u0430\u0439\u043B\u043E\u0432\u2026"
title: "\u0420\u0430\u0431\u043E\u0442\u0430 \u0441 YAML"
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
