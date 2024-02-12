---
title:                "Sắp xếp mã thành các hàm"
aliases: - /vi/java/organizing-code-into-functions.md
date:                  2024-01-28T22:03:05.660892-07:00
model:                 gpt-4-0125-preview
simple_title:         "Sắp xếp mã thành các hàm"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/java/organizing-code-into-functions.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cái gì & Tại sao?
Tổ chức code thành các hàm nghĩa là phân rã một chương trình khổng lồ thành các phần nhỏ quản lý được, mỗi phần thực hiện một tác vụ rõ ràng. Lập trình viên làm điều này để code dễ đọc, dễ tái sử dụng và dễ bảo trì.

## Làm thế nào:
Dưới đây là một ví dụ điển hình - một hàm để tính giai thừa của một số.

```java
public class MathUtils {

    public static void main(String[] args) {
        int number = 5;
        int result = factorial(number);
        System.out.println("Giai thừa của " + number + " là: " + result);
    }
    
    public static int factorial(int n) {
        if (n <= 1) {
            return 1;
        }
        return n * factorial(n - 1);
    }
}
```

Kết quả sẽ là:
```
Giai thừa của 5 là: 120
```

## Sâu hơn nữa
Trước khi các hàm trở thành một phần, code được nhồi nhét vào trong các khối đơn khối, khiến việc gỡ lỗi giống như việc tìm kim trong đống rơm. Bây giờ, việc gói gọn chức năng vào trong các hàm giúp xác định vấn đề nhanh chóng. Các phương án thay thế bao gồm biểu thức lambda trong Java hoặc phương thức trong lập trình hướng đối tượng, cả hai đều phục vụ các mục đích tương tự. Khi bạn viết một hàm, hãy nhớ: (1) Mỗi hàm nên có một trách nhiệm duy nhất và (2) tên của hàm nên mô tả rõ ràng mục đích của nó.

## Xem thêm
Để biết thêm về tổ chức code:
- Clean Code bởi Robert C. Martin
- Refactoring: Improving the Design of Existing Code bởi Martin Fowler
- [Tài liệu Oracle Java về Định nghĩa Phương thức](https://docs.oracle.com/javase/tutorial/java/javaOO/methods.html)
