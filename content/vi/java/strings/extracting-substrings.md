---
title:                "Trích xuất chuỗi con"
aliases:
- /vi/java/extracting-substrings.md
date:                  2024-01-28T22:00:44.952495-07:00
model:                 gpt-4-0125-preview
simple_title:         "Trích xuất chuỗi con"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/java/extracting-substrings.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cái gì & Tại sao?

Việc trích xuất các chuỗi con có nghĩa là lấy ra một phần cụ thể từ một chuỗi - một chuỗi các ký tự nằm trong một chuỗi lớn hơn. Các lập trình viên cắt và chia nhỏ các chuỗi để xác nhận đầu vào, phân tích dữ liệu, hoặc đơn giản chỉ để hiển thị những phần liên quan tới người dùng.

## Làm thế nào:

Việc trích xuất một chuỗi con trong Java khá đơn giản sử dụng phương thức `substring`. Dưới đây là cách bạn thực hiện:

```java
public class SubstringExample {
    public static void main(String[] args) {
        String fullString = "Hello, World!";

        // Trích xuất từ chỉ số 7 đến hết chuỗi
        String sub1 = fullString.substring(7);
        System.out.println(sub1); // Kết quả: World!

        // Trích xuất từ chỉ số 0 đến chỉ số 4 (5 không được bao gồm)
        String sub2 = fullString.substring(0, 5);
        System.out.println(sub2); // Kết quả: Hello
    }
}
```

**Nhớ nhé**: Trong Java, chỉ số của chuỗi bắt đầu từ 0.

## Sâu xa hơn

Phương thức `substring` đã có mặt từ những ngày đầu của Java, cung cấp một cách đơn giản để lấy các phần của chuỗi. Trong các phiên bản cũ của Java, `substring` sẽ chia sẻ mảng ký tự gốc, có thể dẫn đến rò rỉ bộ nhớ nếu chuỗi gốc lớn và chuỗi con được giữ lại trong một khoảng thời gian dài. Kể từ bản cập nhật Java 7 phiên bản 6, `substring` tạo ra một chuỗi mới, vì vậy chuỗi cũ có thể được thu gom rác nếu không được sử dụng ở nơi khác.

Ngoài ra, trước khi tiếp cận `substring`, bạn nên xem xét liệu bạn có thể sử dụng `split`, `replace`, hoặc tiện ích regex cho các kịch bản phức tạp hơn. Về nội bộ, `substring` trong Java sử dụng các phương thức từ lớp `String` mà sao chép mảng—hiệu quả, nhưng không phải là thứ bạn có thể kiểm soát trực tiếp.

## Xem thêm

- Để có cái nhìn toàn diện về những gì bạn có thể làm với chuỗi trong Java, hãy xem tài liệu của lớp `String`: [String (Java SE 15 & JDK 15)](https://docs.oracle.com/en/java/javase/15/docs/api/java.base/java/lang/String.html)
- Muốn tìm hiểu sâu hơn về việc thao tác chuỗi phức tạp? Các lớp `Pattern` và `Matcher` sẽ là bạn đồng hành của bạn: [Pattern (Java SE 15 & JDK 15)](https://docs.oracle.com/en/java/javase/15/docs/api/java.base/java/util/regex/Pattern.html)
- Một hướng dẫn về việc sử dụng biểu thức chính quy trong Java: [Regular Expressions](https://docs.oracle.com/javase/tutorial/essential/regex/)

Cho dù là để cắt gọn nhanh chóng hay trích xuất dữ liệu phức tạp, những chức năng bạn cần đều ở đó. Hãy hiểu rõ bộ công cụ của mình và sẵn sàng sử dụng bất cứ lúc nào.
