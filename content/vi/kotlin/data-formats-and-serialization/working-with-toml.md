---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:11:25.270391-07:00
description: "L\xE0m th\u1EBF n\xE0o: \u0110\u1EC3 x\u1EED l\xFD TOML trong Kotlin,\
  \ b\u1EA1n c\xF3 th\u1EC3 s\u1EED d\u1EE5ng m\u1ED9t th\u01B0 vi\u1EC7n nh\u01B0\
  \ `ktoml`. \u0110\u1EA7u ti\xEAn, h\xE3y th\xEAm ph\u1EE5 thu\u1ED9c v\xE0o `build.gradle.kts`\
  \ c\u1EE7a b\u1EA1n."
lastmod: '2024-03-13T22:44:36.631975-06:00'
model: gpt-4-0125-preview
summary: "\u0110\u1EC3 x\u1EED l\xFD TOML trong Kotlin, b\u1EA1n c\xF3 th\u1EC3 s\u1EED\
  \ d\u1EE5ng m\u1ED9t th\u01B0 vi\u1EC7n nh\u01B0 `ktoml`."
title: "L\xE0m vi\u1EC7c v\u1EDBi TOML"
weight: 39
---

## Làm thế nào:
Để xử lý TOML trong Kotlin, bạn có thể sử dụng một thư viện như `ktoml`. Đầu tiên, hãy thêm phụ thuộc vào `build.gradle.kts` của bạn:

```kotlin
dependencies {
    implementation("com.akuleshov7:ktoml:0.2.5")
}
```

Bây giờ, hãy phân tích một số TOML:

```kotlin
import com.akuleshov7.ktoml.file.TomlFileReader

fun main() {
    val tomlContent = TomlFileReader.readAndParseFile("config.toml")
    
    val caiDatCSDL = tomlContent.getTable("database")
    val host = caiDatCSDL.getString("host")
    val port = caiDatCSDL.getLong("port")

    println("CSDL Máy chủ: $host")
    println("CSDL Cổng: $port")
}
```

Giả sử `config.toml` trông như thế này:

```toml
[database]
host = "localhost"
port = 5432
```

Mẫu đầu ra sẽ là:

```
CSDL Máy chủ: localhost
CSDL Cổng: 5432
```

## Đào Sâu
TOML, được tạo ra bởi đồng sáng lập GitHub Tom Preston-Werner vào năm 2013, nhằm mục đích đơn giản hơn YAML và an toàn hơn về kiểu dữ liệu so với JSON. Nó đã trở nên phổ biến, đặc biệt với `Cargo` của Rust và hệ thống mô-đun của Go. Các lựa chọn khác? YAML có nhiều tính năng hơn, JSON được chuyển thẳng vào đối tượng trong nhiều ngôn ngữ lập trình, và luôn luôn có XML đáng tin cậy. Về việc triển khai, ktoml, dưới giấy phép Apache 2.0, là một thư viện Kotlin thuần túy và không kéo theo thư viện Java, cung cấp DSL để viết TOML, không chỉ đọc.

## Xem Thêm
- GitHub của TOML: https://github.com/toml-lang/toml
- GitHub của ktoml: https://github.com/akuleshov7/ktoml
- So sánh TOML vs. YAML vs. JSON: https://blog.logrocket.com/comparing-configuration-files-yaml-toml-json/
