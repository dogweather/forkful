---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:11:40.952032-07:00
description: "\u0110i\u1EC1u g\xEC & T\u1EA1i sao? L\xE0m vi\u1EC7c v\u1EDBi JSON\
  \ (JavaScript Object Notation) c\xF3 ngh\u0129a l\xE0 b\u1EA1n \u0111ang x\u1EED\
  \ l\xFD \u0111\u1ECBnh d\u1EA1ng trao \u0111\u1ED5i d\u1EEF li\u1EC7u nh\u1EB9 n\xE0\
  y trong c\xE1c \u1EE9ng d\u1EE5ng Java\u2026"
lastmod: '2024-04-05T21:53:37.920104-06:00'
model: gpt-4-0125-preview
summary: "L\xE0m vi\u1EC7c v\u1EDBi JSON (JavaScript Object Notation) c\xF3 ngh\u0129\
  a l\xE0 b\u1EA1n \u0111ang x\u1EED l\xFD \u0111\u1ECBnh d\u1EA1ng trao \u0111\u1ED5\
  i d\u1EEF li\u1EC7u nh\u1EB9 n\xE0y trong c\xE1c \u1EE9ng d\u1EE5ng Java c\u1EE7\
  a m\xECnh."
title: "L\xE0m vi\u1EC7c v\u1EDBi JSON"
weight: 38
---

### Điều gì & Tại sao?
Làm việc với JSON (JavaScript Object Notation) có nghĩa là bạn đang xử lý định dạng trao đổi dữ liệu nhẹ này trong các ứng dụng Java của mình. Lập trình viên chọn JSON để tuần tự hóa và truyền tải dữ liệu có cấu trúc qua mạng cũng như dễ dàng cấu hình và lưu trữ dữ liệu vì nó dễ đọc và độc lập với ngôn ngữ.

### Cách thực hiện:
Hãy xắn tay áo lên và bắt đầu code với JSON trong Java.

Đầu tiên, bạn cần một thư viện xử lý JSON như `Jackson` hoặc `Google Gson`. Ở đây chúng ta sẽ sử dụng `Jackson`, vì vậy hãy thêm dependency này vào `pom.xml` của bạn:

```xml
<dependency>
    <groupId>com.fasterxml.jackson.core</groupId>
    <artifactId>jackson-databind</artifactId>
    <version>2.13.1</version>
</dependency>
```

Bây giờ, hãy tuần tự hóa (ghi) một đối tượng Java đơn giản thành JSON:

```java
import com.fasterxml.jackson.databind.ObjectMapper;

public class JsonExample {
    public static void main(String[] args) {
        try {
            ObjectMapper mapper = new ObjectMapper();
            Person person = new Person("Alex", 30);
            String json = mapper.writeValueAsString(person);
            System.out.println(json);
        } catch (Exception e) {
            e.printStackTrace();
        }
    }
}

class Person {
    public String name;
    public int age;

    public Person(String name, int age) {
        this.name = name;
        this.age = age;
    }
}
```

Kết quả sẽ là:

```json
{"name":"Alex","age":30}
```

Bây giờ, để giải tuần tự hóa (đọc) JSON trở lại thành một đối tượng Java:

```java
import com.fasterxml.jackson.databind.ObjectMapper;

public class JsonExample {
    public static void main(String[] args) {
        String json = "{\"name\":\"Alex\",\"age\":30}";
        try {
            ObjectMapper mapper = new ObjectMapper();
            Person person = mapper.readValue(json, Person.class);
            System.out.println(person.name + " is " + person.age + " tuổi.");
        } catch (Exception e) {
            e.printStackTrace();
        }
    }
}
```

Kết quả sẽ là:

```
Alex is 30 tuổi.
```

### Khám phá sâu hơn
Sự đơn giản và hiệu quả của JSON đã khiến nó trở thành chuẩn mặc định cho trao đổi dữ liệu trên web, lật đổ ngai vàng của XML. Được giới thiệu vào đầu những năm 2000, JSON được phát triển từ JavaScript nhưng hiện được hỗ trợ trên hầu hết các ngôn ngữ.

Những phương án khác cho JSON bao gồm XML, có cú pháp dài dòng hơn, và các định dạng nhị phân như Protocol Buffers hoặc MessagePack, ít dễ đọc hơn nhưng hiệu quả hơn về kích thước và tốc độ. Mỗi loại có những trường hợp sử dụng riêng; sự lựa chọn tùy thuộc vào nhu cầu và bối cảnh cụ thể của dữ liệu bạn.

Trong Java, ngoài `Jackson` và `Gson`, chúng ta còn có `JsonB` và `org.json` là những thư viện khác để xử lý JSON. Jackson cung cấp xử lý dựa trên luồng và được biết đến với tốc độ, trong khi Gson được ca ngợi vì sự dễ sử dụng của nó. JsonB là một phần của Jakarta EE, cung cấp một cách tiếp cận tiêu chuẩn hóa.

Khi triển khai JSON, hãy nhớ xử lý ngoại lệ một cách thích hợp - mã của bạn nên đủ mạnh để chống lại dữ liệu đầu vào xấu. Ngoài ra, hãy xem xét đến ảnh hưởng về bảo mật của việc ràng buộc dữ liệu tự động – luôn kiểm tra đầu vào của bạn!

### Xem thêm
- [Dự án Jackson](https://github.com/FasterXML/jackson)
- [Dự án Gson](https://github.com/google/gson)
- [Quy cách JSON](https://www.json.org/json-en.html)
- [Quy cách JsonB](https://jakarta.ee/specifications/jsonb/)
