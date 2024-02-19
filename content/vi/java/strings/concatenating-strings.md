---
aliases:
- /vi/java/concatenating-strings/
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:57:35.229393-07:00
description: "N\u1ED1i chu\u1ED7i c\xF3 ngh\u0129a l\xE0 gh\xE9p ch\xFAng l\u1EA1\
  i v\u1EDBi nhau t\u1EEB \u0111\u1EA7u \u0111\u1EBFn cu\u1ED1i \u0111\u1EC3 t\u1EA1\
  o th\xE0nh m\u1ED9t chu\u1ED7i m\u1EDBi. \u0110i\u1EC1u n\xE0y r\u1EA5t ti\u1EC7\
  n l\u1EE3i \u0111\u1EC3 t\u1EA1o ra c\xE1c th\xF4ng \u0111i\u1EC7p t\xF9y ch\u1EC9\
  nh, x\xE2y\u2026"
lastmod: 2024-02-18 23:08:50.547966
model: gpt-4-0125-preview
summary: "N\u1ED1i chu\u1ED7i c\xF3 ngh\u0129a l\xE0 gh\xE9p ch\xFAng l\u1EA1i v\u1EDB\
  i nhau t\u1EEB \u0111\u1EA7u \u0111\u1EBFn cu\u1ED1i \u0111\u1EC3 t\u1EA1o th\xE0\
  nh m\u1ED9t chu\u1ED7i m\u1EDBi. \u0110i\u1EC1u n\xE0y r\u1EA5t ti\u1EC7n l\u1EE3\
  i \u0111\u1EC3 t\u1EA1o ra c\xE1c th\xF4ng \u0111i\u1EC7p t\xF9y ch\u1EC9nh, x\xE2\
  y\u2026"
title: "N\u1ED1i chu\u1ED7i k\xFD t\u1EF1"
---

{{< edit_this_page >}}

## Cái gì & Tại sao?

Nối chuỗi có nghĩa là ghép chúng lại với nhau từ đầu đến cuối để tạo thành một chuỗi mới. Điều này rất tiện lợi để tạo ra các thông điệp tùy chỉnh, xây dựng văn bản để xuất ra, hoặc xử lý nhập liệu từ người dùng.

## Làm thế nào:

Dưới đây là cách nối chuỗi trong Java:

```java
public class StringConcatenationDemo {
    public static void main(String[] args) {
        String firstName = "John";
        String lastName = "Doe";
        
        // Sử dụng toán tử cộng
        String fullName = firstName + " " + lastName;
        System.out.println(fullName); // Kết quả: John Doe
        
        // Sử dụng phương thức concat()
        String anotherFullName = firstName.concat(" ").concat(lastName);
        System.out.println(anotherFullName); // Kết quả: John Doe
        
        // Sử dụng StringBuilder cho nhiều phép nối
        StringBuilder builder = new StringBuilder();
        builder.append(firstName).append(" ").append(lastName);
        System.out.println(builder.toString()); // Kết quả: John Doe
    }
}
```

## Tìm hiểu sâu

Việc nối chuỗi có vẻ đơn giản, phải không? Nó đã có mặt trong Java từ những ngày đầu, và chúng ta có một vài cách để thực hiện. Các phiên bản Java sớm sử dụng `StringBuilder` ngầm định mỗi khi bạn thực hiện phép cộng `+`. Sau đó, Java 5 ra đời, và mọi thứ trở nên hiệu quả hơn với sự giới thiệu của `StringJoiner` và nhiều cải tiến cho lớp `StringBuilder`.

Bây giờ, bạn có thể tự hỏi tại sao không luôn sử dụng toán tử `+` nếu nó giống nhau? Hóa ra, `+` rất tốt cho một công việc nhanh chóng với những chuỗi nhỏ hoặc một vài phép nối. Tuy nhiên, đằng sau hậu trường, nó có thể trở nên tốn kém về hiệu suất nếu bạn sử dụng nó trong một vòng lặp với nhiều lần lặp lại vì nó tạo ra các đối tượng tạm thời trước khi đạt được phiên bản chuỗi cuối cùng.

Trong những trường hợp nặng nề đó, `StringBuilder` hoặc `StringBuffer` sẽ ra đời. `StringBuilder` thường nhanh hơn vì nó không được đồng bộ hóa—khiến nó không an toàn với luồng nhưng nhanh chóng. `StringBuffer` là lựa chọn cũ hơn, an toàn với luồng. Nó chậm hơn do gánh nặng đồng bộ hóa. Chọn dựa trên nhu cầu về sự an toàn của luồng của bạn.

Đối với phương thức `concat()`, nó khá đơn giản nhưng không linh hoạt bằng `StringBuilder`. Bạn muốn lặp và tiếp tục thêm nhiều chuỗi? `concat()` ít tiện lợi hơn.

Tính đến Java 8 trở đi, chúng ta cũng có `String.join()`, rất gọn cho việc kết nối các bộ sưu tập chuỗi với một dấu phân cách.

## Xem Thêm

- [Tài liệu lớp `String`](https://docs.oracle.com/en/java/javase/17/docs/api/java.base/java/lang/String.html)
- [Tài liệu lớp `StringBuilder`](https://docs.oracle.com/en/java/javase/17/docs/api/java.base/java/lang/StringBuilder.html)
- [Tài liệu lớp `StringBuffer`](https://docs.oracle.com/en/java/javase/17/docs/api/java.base/java/lang/StringBuffer.html)
- [Hướng dẫn Java của Oracle về Chuỗi](https://docs.oracle.com/javase/tutorial/java/data/strings.html)
