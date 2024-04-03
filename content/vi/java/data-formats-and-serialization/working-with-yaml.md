---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:12:17.943551-07:00
description: "L\xE0m Th\u1EBF N\xE0o: \u0110\u1EC3 x\u1EED l\xFD YAML trong Java,\
  \ h\xE3y s\u1EED d\u1EE5ng `snakeyaml`, m\u1ED9t th\u01B0 vi\u1EC7n ph\u1ED5 bi\u1EBF\
  n. \u0110\u1EA7u ti\xEAn, th\xEAm s\u1EF1 ph\u1EE5 thu\u1ED9c v\xE0o `pom.xml` c\u1EE7\
  a b\u1EA1n."
lastmod: '2024-03-13T22:44:36.516046-06:00'
model: gpt-4-0125-preview
summary: "\u0110\u1EC3 x\u1EED l\xFD YAML trong Java, h\xE3y s\u1EED d\u1EE5ng `snakeyaml`,\
  \ m\u1ED9t th\u01B0 vi\u1EC7n ph\u1ED5 bi\u1EBFn."
title: "L\xE0m vi\u1EC7c v\u1EDBi YAML"
weight: 41
---

## Làm Thế Nào:
Để xử lý YAML trong Java, hãy sử dụng `snakeyaml`, một thư viện phổ biến.

Đầu tiên, thêm sự phụ thuộc vào `pom.xml` của bạn:

```xml
<dependency>
    <groupId>org.yaml</groupId>
    <artifactId>snakeyaml</artifactId>
    <version>1.29</version>
</dependency>
```

Bây giờ, đọc một tệp YAML:

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

Giả sử `config.yaml` trông như thế này:

```yaml
version: '1.0'
services:
  webapp:
    build: .
    ports:
      - "5000:5000"
```

Đầu ra sẽ là biểu diễn `Map` của YAML của bạn:

```
{version=1.0, services={webapp={build=., ports=[5000:5000]}}}
```

Bây giờ, hãy viết YAML:

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

Kiểm tra `output.yaml` để xem nội dung YAML mới:

```yaml
name: myapp
version: '2.0'
```

## Sâu Hơn
YAML xuất hiện vào đầu những năm 2000 như một lựa chọn thay thế XML cho việc cấu trúc dữ liệu đơn giản hơn. Mặc dù sự phát triển của JSON đã làm lu mờ nó trong giao tiếp API, tính thân thiện với con người của YAML giữ cho nó phổ biến cho cấu hình. Cùng một dữ liệu, nhưng JSON và TOML là các lựa chọn thay thế cho YAML, tùy thuộc vào trường hợp sử dụng. Một lưu ý về YAML: không cho phép sử dụng tab để thụt lề; chỉ dùng khoảng trắng.

## Xem Thêm
Khám phá thêm với các tài nguyên này:

- Bản Spec YAML Chính Thức: https://yaml.org/spec/1.2.2/
- Kho GitHub snakeyaml: https://github.com/asomov/snakeyaml
- YAML vs JSON: https://phoenixnap.com/kb/yaml-vs-json
- YAML Lint, để xác thực các tệp YAML của bạn: http://www.yamllint.com/
