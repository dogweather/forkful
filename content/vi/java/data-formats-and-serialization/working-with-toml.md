---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:11:10.390249-07:00
description: "C\xE1ch th\u1EF1c hi\u1EC7n: B\u1EA1n s\u1EBD c\u1EA7n m\u1ED9t th\u01B0\
  \ vi\u1EC7n ph\xE2n t\xEDch c\xFA ph\xE1p TOML. T\xF4i khuy\u1EBFn ngh\u1ECB s\u1EED\
  \ d\u1EE5ng `toml4j`. Th\xEAm n\xF3 v\xE0o d\u1EF1 \xE1n c\u1EE7a b\u1EA1n nh\u01B0\
  \ sau."
lastmod: '2024-03-13T22:44:36.519314-06:00'
model: gpt-4-0125-preview
summary: "B\u1EA1n s\u1EBD c\u1EA7n m\u1ED9t th\u01B0 vi\u1EC7n ph\xE2n t\xEDch c\xFA\
  \ ph\xE1p TOML."
title: "L\xE0m vi\u1EC7c v\u1EDBi TOML"
weight: 39
---

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
