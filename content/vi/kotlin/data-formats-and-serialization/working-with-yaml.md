---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:12:22.480444-07:00
description: "L\xE0m th\u1EBF n\xE0o: \u0110\u1EC3 l\xE0m vi\u1EC7c v\u1EDBi YAML\
  \ trong Kotlin, b\u1EA1n th\u01B0\u1EDDng s\u1EED d\u1EE5ng m\u1ED9t th\u01B0 vi\u1EC7\
  n nh\u01B0 `snakeyaml`. H\xE3y c\xF9ng t\xECm hi\u1EC3u c\xE1ch ph\xE2n t\xEDch\
  \ m\u1ED9t t\u1EC7p YAML: \u0110\u1EA7u ti\xEAn,\u2026"
lastmod: '2024-03-13T22:44:36.628241-06:00'
model: gpt-4-0125-preview
summary: "\u0110\u1EC3 l\xE0m vi\u1EC7c v\u1EDBi YAML trong Kotlin, b\u1EA1n th\u01B0\
  \u1EDDng s\u1EED d\u1EE5ng m\u1ED9t th\u01B0 vi\u1EC7n nh\u01B0 `snakeyaml`."
title: "L\xE0m vi\u1EC7c v\u1EDBi YAML"
weight: 41
---

## Làm thế nào:
Để làm việc với YAML trong Kotlin, bạn thường sử dụng một thư viện như `snakeyaml`. Hãy cùng tìm hiểu cách phân tích một tệp YAML:

Đầu tiên, thêm phụ thuộc vào tệp `build.gradle` của bạn:

```kotlin
implementation("org.yaml:snakeyaml:1.29")
```

Bây giờ chúng ta cùng phân tích một tệp YAML đơn giản sử dụng SnakeYAML:

```kotlin
import org.yaml.snakeyaml.Yaml
import java.io.InputStream

fun main() {
    val yaml = Yaml()
    val inputStream: InputStream = this::class.java.classLoader.getResourceAsStream("config.yaml")
    val data: Map<String, Any> = yaml.load(inputStream)

    println(data["name"])
    println(data["age"])
}

// Nội dung mẫu của config.yaml:
// name: John Doe
// age: 30

// Kết quả mẫu:
// John Doe
// 30
```

Đoạn mã này tải một tệp YAML và in ra các giá trị liên quan đến các khóa `name` và `age`.

## Đi sâu
YAML xuất hiện vào đầu những năm 2000 để chống lại sự phức tạp của XML. Nó cung cấp một cú pháp đơn giản hơn, làm cho nó được ưa chuộng cho các tệp cấu hình. Các lựa chọn thay thế bao gồm JSON, định hướng dữ liệu nhiều hơn và ít thân thiện với con người hơn, và TOML, đâu đó là một điểm giữa. Khi xử lý YAML trong Kotlin, các thư viện như `snakeyaml` cung cấp động cơ phân tích cú pháp, kết nối với mã Kotlin của bạn để chuyển đổi chuỗi YAML thành cấu trúc dữ liệu gốc.

## Xem thêm
- Đặc tả YAML 1.2: [https://yaml.org/spec/1.2/spec.html](https://yaml.org/spec/1.2/spec.html)
- Tài liệu Kotlin: [https://kotlinlang.org/docs/home.html](https://kotlinlang.org/docs/home.html)
