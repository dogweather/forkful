---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:26:04.813632-07:00
description: "YAML, \u0449\u043E \u0454 \u0441\u043A\u043E\u0440\u043E\u0447\u0435\
  \u043D\u043D\u044F\u043C \u0432\u0456\u0434 \"YAML Ain't Markup Language\", \u0446\
  \u0435 \u0441\u0442\u0430\u043D\u0434\u0430\u0440\u0442 \u0441\u0435\u0440\u0456\
  \u0430\u043B\u0456\u0437\u0430\u0446\u0456\u0457 \u0434\u0430\u043D\u0438\u0445\
  , \u044F\u043A\u0438\u0439 \u0454 \u0437\u0440\u0443\u0447\u043D\u0438\u043C \u0434\
  \u043B\u044F \u0441\u043F\u0440\u0438\u0439\u043D\u044F\u0442\u0442\u044F \u043B\
  \u044E\u0434\u0438\u043D\u043E\u044E, \u044F\u043A\u0438\u0439 \u043F\u0440\u043E\
  \u0433\u0440\u0430\u043C\u0456\u0441\u0442\u0438\u2026"
lastmod: '2024-03-13T22:44:49.114180-06:00'
model: gpt-4-0125-preview
summary: "YAML, \u0449\u043E \u0454 \u0441\u043A\u043E\u0440\u043E\u0447\u0435\u043D\
  \u043D\u044F\u043C \u0432\u0456\u0434 \"YAML Ain't Markup Language\", \u0446\u0435\
  \ \u0441\u0442\u0430\u043D\u0434\u0430\u0440\u0442 \u0441\u0435\u0440\u0456\u0430\
  \u043B\u0456\u0437\u0430\u0446\u0456\u0457 \u0434\u0430\u043D\u0438\u0445, \u044F\
  \u043A\u0438\u0439 \u0454 \u0437\u0440\u0443\u0447\u043D\u0438\u043C \u0434\u043B\
  \u044F \u0441\u043F\u0440\u0438\u0439\u043D\u044F\u0442\u0442\u044F \u043B\u044E\
  \u0434\u0438\u043D\u043E\u044E, \u044F\u043A\u0438\u0439 \u043F\u0440\u043E\u0433\
  \u0440\u0430\u043C\u0456\u0441\u0442\u0438\u2026"
title: "\u0420\u043E\u0431\u043E\u0442\u0430 \u0437 YAML"
---

{{< edit_this_page >}}

## Що та чому?
YAML, що є скороченням від "YAML Ain't Markup Language", це стандарт серіалізації даних, який є зручним для сприйняття людиною, який програмісти використовують для файлів конфігурації, вивантаження даних та передачі даних між мовами. Він популярний завдяки своїй читабельності та простоті використання, що робить його поширеним вибором для конфігурування додатків та сервісів.

## Як це зробити:
У Java ви можете працювати з YAML-файлами за допомогою сторонніх бібліотек, оскільки Java Standard Edition не включає вбудовану підтримку для YAML. Однією з популярних бібліотек є SnakeYAML, яка дозволяє легко аналізувати та генерувати дані YAML.

### Налаштування SnakeYAML
Спочатку додайте SnakeYAML у свій проект. Якщо ви користуєтесь Maven, додайте таку залежність до вашого `pom.xml`:

```xml
<dependency>
    <groupId>org.yaml</groupId>
    <artifactId>snakeyaml</artifactId>
    <version>1.30</version>
</dependency>
```

### Читання YAML
```java
import org.yaml.snakeyaml.Yaml;
import java.io.InputStream;
import java.util.Map;

public class ReadYamlExample {
    public static void main(String[] args) {
        Yaml yaml = new Yaml();
        try (InputStream inputStream = ReadYamlExample.class
                .getClassLoader()
                .getResourceAsStream("config.yml")) {
            Map<String, Object> data = yaml.load(inputStream);
            System.out.println(data);
        } catch (Exception e) {
            e.printStackTrace();
        }
    }
}
```
Припустимо, `config.yml` виглядає так:
```yaml
name: Example
version: 1.0
features:
  - login
  - signup
```
Вивід буде:
```
{name=Example, version=1.0, features=[login, signup]}
```

### Запис YAML
Щоб згенерувати YAML із об'єктів Java, використовуйте метод `dump`, який надається SnakeYAML:
```java
import org.yaml.snakeyaml.Yaml;
import java.util.Arrays;
import java.util.LinkedHashMap;
import java.util.Map;

public class WriteYamlExample {
    public static void main(String[] args) {
        Map<String, Object> data = new LinkedHashMap<>();
        data.put("name", "Example");
        data.put("version", 1.0);
        data.put("features", Arrays.asList("login", "signup"));

        Yaml yaml = new Yaml();
        String output = yaml.dump(data);
        System.out.println(output);
    }
}
```
Це згенерує та виведе такий вміст YAML:
```yaml
name: Example
version: 1.0
features:
- login
- signup
```
Використовуючи SnakeYAML, розробники на Java легко можуть інтегрувати аналіз та генерацію YAML у свої додатки, користуючись читабельністю та простотою YAML для конфігурації та обміну даними.
