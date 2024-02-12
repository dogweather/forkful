---
title:                "Làm việc với TOML"
aliases:
- /vi/java/working-with-toml.md
date:                  2024-01-28T22:11:10.390249-07:00
model:                 gpt-4-0125-preview
simple_title:         "Làm việc với TOML"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/java/working-with-toml.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Là gì & Tại sao?
TOML là viết tắt của Tom's Obvious, Minimal Language. Đây là một định dạng chuỗi hóa dữ liệu được sử dụng cho các tệp cấu hình. Lập trình viên sử dụng nó bởi vì nó dễ đọc, dễ viết và dễ ánh xạ sang một bảng băm.

## Cách thực hiện:
Bạn sẽ cần một thư viện phân tích cú pháp TOML. Tôi khuyến nghị sử dụng `toml4j`. Thêm nó vào dự án của bạn như sau:

```java
// Thêm vào build.gradle của bạn
dependencies {
    implementation 'com.moandjiezana.toml:toml4j:0.7.2'
}
```

Dưới đây là cách phân tích một tệp TOML:

```java
import com.moandjiezana.toml.Toml;

public class TomlExample {
    public static void main(String[] args) {
        Toml toml = new Toml().read("""
            [server]
            ip = "192.168.1.1"
            port = 80
            """);

        String ip = toml.getString("server.ip");
        Integer port = toml.getLong("server.port").intValue();
        
        System.out.println("Địa chỉ IP của Máy chủ: " + ip);
        System.out.println("Cổng của Máy chủ: " + port);
    }
}
```

Kết quả mẫu:

```
Địa chỉ IP của Máy chủ: 192.168.1.1
Cổng của Máy chủ: 80
```

## Sâu hơn nữa
Được phát triển bởi đồng sáng lập GitHub, Tom Preston-Werner, TOML nhằm mục tiêu đơn giản hơn XML và cụ thể hơn YAML. Phiên bản mới nhất 1.0.0, được phát hành vào năm 2021, cung cấp một bộ tính năng ổn định.

Các lựa chọn thay thế như JSON hoặc YAML cũng phổ biến. JSON tuyệt vời cho việc trao đổi dữ liệu. YAML dễ đọc hơn cho các cấu hình phức tạp. Sức mạnh của TOML nằm ở sự đơn giản trực tiếp và việc sử dụng nó trong cộng đồng Rust.

Về việc thực hiện, khi sử dụng TOML với Java, hãy nhớ rằng trình phân tích cú pháp bạn chọn quan trọng. Ngoài `toml4j`, một số người chọn `jackson-dataformat-toml`. Mỗi cái sẽ có những điểm tinh tế, như xử lý lỗi hoặc hiệu suất phân tích cú pháp, vì vậy hãy chọn dựa trên nhu cầu của dự án bạn.

## Xem thêm
- Đặc tả TOML: https://toml.io/en/
- GitHub `toml4j`: https://github.com/mwanji/toml4j
- `jackson-dataformat-toml`: https://github.com/FasterXML/jackson-dataformats-text/tree/main/toml
