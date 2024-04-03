---
changelog:
- 2024-01-29, gpt-4-0125-preview, translated from English
date: 2024-01-29 00:05:31.735896-07:00
description: "\u041A\u0430\u043A \u044D\u0442\u043E \u0441\u0434\u0435\u043B\u0430\
  \u0442\u044C: \u0427\u0442\u043E\u0431\u044B \u0440\u0430\u0431\u043E\u0442\u0430\
  \u0442\u044C \u0441 YAML \u0432 Java, \u0434\u0430\u0432\u0430\u0439\u0442\u0435\
  \ \u0438\u0441\u043F\u043E\u043B\u044C\u0437\u043E\u0432\u0430\u0442\u044C `snakeyaml`,\
  \ \u043F\u043E\u043F\u0443\u043B\u044F\u0440\u043D\u0443\u044E \u0431\u0438\u0431\
  \u043B\u0438\u043E\u0442\u0435\u043A\u0443. \u0421\u043D\u0430\u0447\u0430\u043B\
  \u0430 \u0434\u043E\u0431\u0430\u0432\u044C\u0442\u0435 \u0437\u0430\u0432\u0438\
  \u0441\u0438\u043C\u043E\u0441\u0442\u044C \u0432 \u0432\u0430\u0448 `pom.xml`."
lastmod: '2024-03-13T22:44:44.860421-06:00'
model: gpt-4-0125-preview
summary: "\u0427\u0442\u043E\u0431\u044B \u0440\u0430\u0431\u043E\u0442\u0430\u0442\
  \u044C \u0441 YAML \u0432 Java, \u0434\u0430\u0432\u0430\u0439\u0442\u0435 \u0438\
  \u0441\u043F\u043E\u043B\u044C\u0437\u043E\u0432\u0430\u0442\u044C `snakeyaml`,\
  \ \u043F\u043E\u043F\u0443\u043B\u044F\u0440\u043D\u0443\u044E \u0431\u0438\u0431\
  \u043B\u0438\u043E\u0442\u0435\u043A\u0443."
title: "\u0420\u0430\u0431\u043E\u0442\u0430 \u0441 YAML"
weight: 41
---

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
