---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:53:22.923560-06:00
description: "\u0E27\u0E34\u0E18\u0E35\u0E01\u0E32\u0E23: \u0E43\u0E19 Java, \u0E04\
  \u0E38\u0E13\u0E2A\u0E32\u0E21\u0E32\u0E23\u0E16\u0E17\u0E33\u0E07\u0E32\u0E19\u0E01\
  \u0E31\u0E1A\u0E44\u0E1F\u0E25\u0E4C YAML \u0E42\u0E14\u0E22\u0E43\u0E0A\u0E49\u0E44\
  \u0E25\u0E1A\u0E23\u0E32\u0E23\u0E35\u0E48\u0E02\u0E2D\u0E07\u0E1A\u0E38\u0E04\u0E04\
  \u0E25\u0E17\u0E35\u0E48\u0E2A\u0E32\u0E21 \u0E40\u0E19\u0E37\u0E48\u0E2D\u0E07\u0E08\
  \u0E32\u0E01 Java Standard Edition \u0E44\u0E21\u0E48\u0E23\u0E27\u0E21\u0E01\u0E32\
  \u0E23\u0E2A\u0E19\u0E31\u0E1A\u0E2A\u0E19\u0E38\u0E19 YAML \u0E21\u0E32\u0E43\u0E2B\
  \u0E49\u2026"
lastmod: '2024-04-05T21:54:01.716342-06:00'
model: gpt-4-0125-preview
summary: "\u0E43\u0E19 Java, \u0E04\u0E38\u0E13\u0E2A\u0E32\u0E21\u0E32\u0E23\u0E16\
  \u0E17\u0E33\u0E07\u0E32\u0E19\u0E01\u0E31\u0E1A\u0E44\u0E1F\u0E25\u0E4C YAML \u0E42\
  \u0E14\u0E22\u0E43\u0E0A\u0E49\u0E44\u0E25\u0E1A\u0E23\u0E32\u0E23\u0E35\u0E48\u0E02\
  \u0E2D\u0E07\u0E1A\u0E38\u0E04\u0E04\u0E25\u0E17\u0E35\u0E48\u0E2A\u0E32\u0E21 \u0E40\
  \u0E19\u0E37\u0E48\u0E2D\u0E07\u0E08\u0E32\u0E01 Java Standard Edition \u0E44\u0E21\
  \u0E48\u0E23\u0E27\u0E21\u0E01\u0E32\u0E23\u0E2A\u0E19\u0E31\u0E1A\u0E2A\u0E19\u0E38\
  \u0E19 YAML \u0E21\u0E32\u0E43\u0E2B\u0E49 \u0E44\u0E25\u0E1A\u0E23\u0E32\u0E23\u0E35\
  \u0E17\u0E35\u0E48\u0E44\u0E14\u0E49\u0E23\u0E31\u0E1A\u0E04\u0E27\u0E32\u0E21\u0E19\
  \u0E34\u0E22\u0E21\u0E2B\u0E19\u0E36\u0E48\u0E07\u0E04\u0E37\u0E2D SnakeYAML \u0E0B\
  \u0E36\u0E48\u0E07\u0E2D\u0E19\u0E38\u0E0D\u0E32\u0E15\u0E43\u0E2B\u0E49\u0E17\u0E33\
  \u0E01\u0E32\u0E23\u0E41\u0E22\u0E01\u0E27\u0E34\u0E40\u0E04\u0E23\u0E32\u0E30\u0E2B\
  \u0E4C\u0E41\u0E25\u0E30\u0E2A\u0E23\u0E49\u0E32\u0E07\u0E02\u0E49\u0E2D\u0E21\u0E39\
  \u0E25 YAML \u0E44\u0E14\u0E49\u0E2D\u0E22\u0E48\u0E32\u0E07\u0E07\u0E48\u0E32\u0E22\
  \u0E14\u0E32\u0E22."
title: "\u0E01\u0E32\u0E23\u0E17\u0E33\u0E07\u0E32\u0E19\u0E01\u0E31\u0E1A YAML"
weight: 41
---

## วิธีการ:
ใน Java, คุณสามารถทำงานกับไฟล์ YAML โดยใช้ไลบรารี่ของบุคคลที่สาม เนื่องจาก Java Standard Edition ไม่รวมการสนับสนุน YAML มาให้ ไลบรารีที่ได้รับความนิยมหนึ่งคือ SnakeYAML ซึ่งอนุญาตให้ทำการแยกวิเคราะห์และสร้างข้อมูล YAML ได้อย่างง่ายดาย

### การตั้งค่า SnakeYAML
เริ่มต้นด้วยการรวม SnakeYAML เข้ากับโปรเจ็กต์ของคุณ หากคุณใช้ Maven, เพิ่มการอ้างอิงต่อไปนี้ลงใน `pom.xml` ของคุณ:

```xml
<dependency>
    <groupId>org.yaml</groupId>
    <artifactId>snakeyaml</artifactId>
    <version>1.30</version>
</dependency>
```

### การอ่าน YAML
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
โดยสมมติไฟล์ `config.yml` ดูเหมือนดังนี้:
```yaml
name: Example
version: 1.0
features:
  - login
  - signup
```
ผลลัพธ์จะเป็น:
```
{name=Example, version=1.0, features=[login, signup]}
```

### การเขียน YAML
ในการสร้าง YAML จากอ็อบเจกต์ Java, ใช้เมธอด `dump` ที่ SnakeYAML ให้มา:
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
สิ่งนี้จะสร้างและพิมพ์เนื้อหา YAML ต่อไปนี้:
```yaml
name: Example
version: 1.0
features:
- login
- signup
```
โดยการใช้ประโยชน์จาก SnakeYAML, นักพัฒนา Java สามารถรวมการทำงานแยกวิเคราะห์และสร้าง YAML เข้ากับแอพพลิเคชันของตนได้อย่างง่ายดาย โดยได้รับประโยชน์จากความสามารถอ่านได้และความเรียบง่ายของ YAML สำหรับวัตถุประสงค์ในการกำหนดค่าและการแลกเปลี่ยนข้อมูล
