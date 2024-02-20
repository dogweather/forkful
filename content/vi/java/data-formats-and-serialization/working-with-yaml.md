---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:12:17.943551-07:00
description: "YAML, \"YAML Ain't Markup Language,\" l\xE0 m\u1ED9t ng\xF4n ng\u1EEF\
  \ tu\u1EA7n t\u1EF1 h\xF3a d\u1EEF li\u1EC7u. L\u1EADp tr\xECnh vi\xEAn s\u1EED\
  \ d\u1EE5ng n\xF3 do t\xEDnh d\u1EC5 \u0111\u1ECDc v\xE0 \u0111\u01A1n gi\u1EA3\
  n, \u0111\u1EB7c bi\u1EC7t l\xE0 cho c\xE1c t\u1EC7p c\u1EA5u\u2026"
lastmod: 2024-02-19 22:04:55.681245
model: gpt-4-0125-preview
summary: "YAML, \"YAML Ain't Markup Language,\" l\xE0 m\u1ED9t ng\xF4n ng\u1EEF tu\u1EA7\
  n t\u1EF1 h\xF3a d\u1EEF li\u1EC7u. L\u1EADp tr\xECnh vi\xEAn s\u1EED d\u1EE5ng\
  \ n\xF3 do t\xEDnh d\u1EC5 \u0111\u1ECDc v\xE0 \u0111\u01A1n gi\u1EA3n, \u0111\u1EB7\
  c bi\u1EC7t l\xE0 cho c\xE1c t\u1EC7p c\u1EA5u\u2026"
title: "L\xE0m vi\u1EC7c v\u1EDBi YAML"
---

{{< edit_this_page >}}

## Gì & Tại Sao?

YAML, "YAML Ain't Markup Language," là một ngôn ngữ tuần tự hóa dữ liệu. Lập trình viên sử dụng nó do tính dễ đọc và đơn giản, đặc biệt là cho các tệp cấu hình, bản sao dữ liệu ban đầu, hoặc như một định dạng giao tiếp giữa các hệ thống khác nhau.

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
