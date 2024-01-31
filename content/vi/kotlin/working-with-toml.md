---
title:                "Làm việc với TOML"
date:                  2024-01-28T22:11:25.270391-07:00
model:                 gpt-4-0125-preview
simple_title:         "Làm việc với TOML"

category:             "Kotlin"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/kotlin/working-with-toml.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Là gì và Tại sao?
TOML là viết tắt của Tom's Obvious, Minimal Language. Ngôn ngữ này được sử dụng cho các tập tin cấu hình vì nó dễ đọc và viết đối với con người, đồng thời vẫn dễ dàng để phân tích đối với máy móc. Các nhà phát triển thường sử dụng TOML để tránh sự lộn xộn của XML và sự tế nhị của JSON khi thiết lập cấu hình.

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
