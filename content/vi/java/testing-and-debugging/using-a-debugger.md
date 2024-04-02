---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:09:48.709262-07:00
description: "Gi\u1EA3 s\u1EED b\u1EA1n c\xF3 m\u1ED9t ch\u01B0\u01A1ng tr\xECnh Java\
  \ \u0111\u01A1n gi\u1EA3n \u0111ang g\u1EB7p s\u1EF1 c\u1ED1, v\xE0 b\u1EA1n kh\xF4\
  ng th\u1EC3 hi\u1EC3u t\u1EA1i sao. D\u01B0\u1EDBi \u0111\xE2y l\xE0 c\xE1ch b\u1EA1\
  n kh\u1EDFi \u0111\u1ED9ng m\u1ED9t debugger s\u1EED d\u1EE5ng Eclipse,\u2026"
lastmod: '2024-03-13T22:44:36.495401-06:00'
model: gpt-4-0125-preview
summary: "Gi\u1EA3 s\u1EED b\u1EA1n c\xF3 m\u1ED9t ch\u01B0\u01A1ng tr\xECnh Java\
  \ \u0111\u01A1n gi\u1EA3n \u0111ang g\u1EB7p s\u1EF1 c\u1ED1, v\xE0 b\u1EA1n kh\xF4\
  ng th\u1EC3 hi\u1EC3u t\u1EA1i sao. D\u01B0\u1EDBi \u0111\xE2y l\xE0 c\xE1ch b\u1EA1\
  n kh\u1EDFi \u0111\u1ED9ng m\u1ED9t debugger s\u1EED d\u1EE5ng Eclipse,\u2026"
title: "S\u1EED d\u1EE5ng b\u1ED9 g\u1EE1 l\u1ED7i"
weight: 35
---

## Cách thực hiện:
Giả sử bạn có một chương trình Java đơn giản đang gặp sự cố, và bạn không thể hiểu tại sao. Dưới đây là cách bạn khởi động một debugger sử dụng Eclipse, một trong những IDE phổ biến cho phát triển Java:

Đầu tiên, hãy chắc chắn bạn đã thiết lập một điểm dừng (breakpoint). Sau đó, nhấp chuột phải vào tệp, chọn 'Debug As', và nhấp vào 'Java Application'.

```Java
public class DebugExample {
    public static void main(String[] args) {
        int a = 5;
        int b = 0;
        // Đặt một điểm dừng ở đây
        int kếtQuả = divide(a, b);
        System.out.println("Kết quả là: " + kếtQuả);
    }

    private static int divide(int tửSố, int mẫuSố) {
        // Một chỗ tốt khác để đặt điểm dừng
        return tửSố / mẫuSố;
    }
}
```

Làm như vậy, chương trình của bạn sẽ tạm dừng tại điểm dừng, và bạn có thể kiểm tra các biến, đi qua mã dòng này qua dòng khác, và theo dõi cách chương trình của bạn hoạt động.

Kết quả mẫu (trong bảng điều khiển debugger):
```
Điểm dừng được kích hoạt tại dòng: int kếtQuả = divide(a, b);
```

## Đào sâu
Khái niệm về việc gỡ lỗi đã tồn tại kể từ những ngày đầu của lập trình. Huyền thoại kể lại rằng thuật ngữ "bug" thực sự đến từ một con bọ cánh mềm thực sự được tìm thấy bên trong một máy tính bởi Grace Hopper, một người tiên phong trong lĩnh vực này. Tiến lên đến ngày nay, và chúng ta có những IDE phức tạp như IntelliJ IDEA, Eclipse, và NetBeans chứa các debugger mạnh mẽ.

Các phương thức thay thế cho debugger của IDE bao gồm việc ghi nhật ký, câu lệnh in (debugger của người nghèo), assertion, và các công cụ gỡ lỗi độc lập như jdb (Java Debugger) là một phần của Bộ phát triển Java (JDK).

Debugger hoạt động bằng cách cho phép lập trình viên tạm dừng thực hiện (breakpoints), đi qua mã, kiểm tra giá trị của biến, sửa đổi giá trị đó một cách trực tiếp, và thậm chí chạy từng khối mã. Việc sử dụng debugger thường được coi là một kỹ thuật vô giá cho việc phát triển các ứng dụng phức tạp, nơi mà việc tìm kiếm chính xác dòng code gây ra sự cố có thể được ví như tìm một cái kim trong đống rơm.

## Xem thêm
- Tài liệu chính thức từ Oracle về việc gỡ lỗi: [Oracle Java SE Debugging](https://docs.oracle.com/javase/8/docs/technotes/tools/windows/jdb.html)
- Hướng dẫn về cách gỡ lỗi của Eclipse: [Eclipse Debugging Tips](https://www.eclipse.org/community/eclipse_newsletter/2017/june/article4.php)
- VisualVM, một công cụ trực quan kết hợp vài công cụ dòng lệnh JDK và khả năng profile nhẹ nhàng: [VisualVM](https://visualvm.github.io/)
