---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:12:22.480444-07:00
description: "YAML, vi\u1EBFt t\u1EAFt c\u1EE7a \"YAML Ain't Markup Language,\" l\xE0\
  \ m\u1ED9t ti\xEAu chu\u1EA9n chu\u1EA9n h\xF3a d\u1EEF li\u1EC7u d\u1EC5 \u0111\
  \u1ECDc cho con ng\u01B0\u1EDDi. L\u1EADp tr\xECnh vi\xEAn s\u1EED d\u1EE5ng n\xF3\
  \ \u0111\u1EC3 c\u1EA5u h\xECnh ph\u1EA7n m\u1EC1m,\u2026"
lastmod: '2024-03-13T22:44:36.628241-06:00'
model: gpt-4-0125-preview
summary: "YAML, vi\u1EBFt t\u1EAFt c\u1EE7a \"YAML Ain't Markup Language,\" l\xE0\
  \ m\u1ED9t ti\xEAu chu\u1EA9n chu\u1EA9n h\xF3a d\u1EEF li\u1EC7u d\u1EC5 \u0111\
  \u1ECDc cho con ng\u01B0\u1EDDi."
title: "L\xE0m vi\u1EC7c v\u1EDBi YAML"
weight: 41
---

## Là gì & Tại sao?

YAML, viết tắt của "YAML Ain't Markup Language," là một tiêu chuẩn chuẩn hóa dữ liệu dễ đọc cho con người. Lập trình viên sử dụng nó để cấu hình phần mềm, định nghĩa dữ liệu, hoặc thiết lập các tham số do tính linh hoạt và dễ đọc của nó.

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
