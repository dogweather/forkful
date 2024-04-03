---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:57:35.229393-07:00
description: "L\xE0m th\u1EBF n\xE0o: D\u01B0\u1EDBi \u0111\xE2y l\xE0 c\xE1ch n\u1ED1\
  i chu\u1ED7i trong Java."
lastmod: '2024-03-13T22:44:36.478558-06:00'
model: gpt-4-0125-preview
summary: "D\u01B0\u1EDBi \u0111\xE2y l\xE0 c\xE1ch n\u1ED1i chu\u1ED7i trong Java."
title: "N\u1ED1i chu\u1ED7i k\xFD t\u1EF1"
weight: 3
---

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
