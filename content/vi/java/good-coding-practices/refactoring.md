---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:06:29.739263-07:00
description: "T\xE1i c\u1EA5u tr\xFAc l\xE0 qu\xE1 tr\xECnh c\u1EA5u tr\xFAc l\u1EA1\
  i m\xE3 m\xE1y t\xEDnh hi\u1EC7n c\xF3\u2014thay \u0111\u1ED5i c\xE1ch ph\xE2n lo\u1EA1\
  i\u2014m\xE0 kh\xF4ng thay \u0111\u1ED5i h\xE0nh vi b\xEAn ngo\xE0i c\u1EE7a n\xF3\
  . L\u1EADp tr\xECnh vi\xEAn l\xE0m \u0111i\u1EC1u n\xE0y\u2026"
lastmod: '2024-03-13T22:44:36.500418-06:00'
model: gpt-4-0125-preview
summary: "T\xE1i c\u1EA5u tr\xFAc l\xE0 qu\xE1 tr\xECnh c\u1EA5u tr\xFAc l\u1EA1i\
  \ m\xE3 m\xE1y t\xEDnh hi\u1EC7n c\xF3\u2014thay \u0111\u1ED5i c\xE1ch ph\xE2n lo\u1EA1\
  i\u2014m\xE0 kh\xF4ng thay \u0111\u1ED5i h\xE0nh vi b\xEAn ngo\xE0i c\u1EE7a n\xF3\
  ."
title: "T\xE1i c\u1EA5u tr\xFAc m\xE3"
weight: 19
---

## Cái gì & Tại sao?
Tái cấu trúc là quá trình cấu trúc lại mã máy tính hiện có—thay đổi cách phân loại—mà không thay đổi hành vi bên ngoài của nó. Lập trình viên làm điều này để cải thiện các thuộc tính phi chức năng của phần mềm, tăng cường khả năng đọc, giảm phức tạp và làm cho mã dễ bảo trì hơn cho các dự án tương lai.

## Làm thế nào:
Hãy xem một lớp Java đơn giản đang cần được tái cấu trúc do tổ chức kém và thiếu rõ ràng.

```java
public class Calculator {
    public int calc(int op1, int op2, String operation) {
        if (operation.equals("add")) {
            return op1 + op2;
        } else if (operation.equals("subtract")) {
            return op1 - op2;
        } // Các phép tính khác...
    }
}
```

Sau khi tái cấu trúc, chúng ta có:

```java
public class Calculator {
    public int add(int operand1, int operand2) {
        return operand1 + operand2;
    }

    public int subtract(int operand1, int operand2) {
        return operand1 - operand2;
    }

    // Các phép tính khác...
}
```

Qua việc tái cấu trúc, chúng ta đã cải thiện tên phương thức và tham số cho tính dễ đọc và loại bỏ nhu cầu về một nhánh điều kiện trong một phương thức duy nhất. Mỗi hoạt động giờ đây đều rõ ràng mục đích của nó.

## Sâu hơn:
Tái cấu trúc có nguồn gốc từ cộng đồng Smalltalk, với trọng tâm vào khả năng đọc mã và thiết kế hướng đối tượng, nhưng nó thực sự bùng nổ trong thế giới Java vào cuối những năm '90 và đầu những năm '00, đặc biệt sau khi xuất bản cuốn sách quan trọng của Martin Fowler, "Tái cấu trúc: Cải thiện Thiết kế của Mã Hiện Hữu."

Có những phương án thay thế cho việc tái cấu trúc, như viết lại mã từ đầu. Tuy nhiên, tái cấu trúc thường được ưa thích vì nó liên quan đến việc thay đổi theo từng bước mà không làm gián đoạn chức năng của ứng dụng.

Chi tiết thực hiện khi tái cấu trúc trong Java (hoặc bất kỳ ngôn ngữ lập trình nào) xoay quanh việc hiểu các mùi mã—chỉ báo về các vấn đề sâu hơn trong mã. Một số mùi bao gồm phương thức dài, lớp lớn, mã trùng lặp, và sử dụng quá mức các kiểu nguyên thủy. Bằng cách áp dụng các mẫu tái cấu trúc như Trích xuất Phương thức, Di chuyển Phương thức, hoặc Thay thế Temp với Truy vấn, nhà phát triển có thể đối phó một cách hệ thống với các mùi này trong khi đảm bảo mã luôn hoạt động mọi lúc.

Các công cụ tự động, như hỗ trợ tái cấu trúc của IntelliJ IDEA, hoặc các plugin cho Eclipse, có thể hỗ trợ quá trình bằng cách tự động thực hiện việc tái cấu trúc như đổi tên biến, phương thức và lớp, trích xuất phương thức hoặc biến, và di chuyển phương thức hoặc lớp đến các gói hoặc không gian tên khác nhau.

## Xem thêm:
- "Tái cấu trúc: Cải thiện Thiết kế của Mã Hiện Hữu" của Martin Fowler: https://martinfowler.com/books/refactoring.html
- Kỹ thuật tái cấu trúc trên Refactoring.Guru: https://refactoring.guru/refactoring/techniques
- Tái cấu trúc tự động trong Eclipse: https://www.eclipse.org/eclipse/news/4.18/jdt.php
- Tính năng tái cấu trúc của IntelliJ IDEA: https://www.jetbrains.com/idea/features/refactoring.html

Mỗi nguồn tài liệu này cung cấp hoặc nền tảng để hiểu nguyên lý của tái cấu trúc hoặc công cụ có thể được tận dụng để đưa nguyên lý này vào thực hành.
