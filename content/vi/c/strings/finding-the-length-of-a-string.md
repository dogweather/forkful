---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 17:57:02.729104-07:00
description: "L\xE0m th\u1EBF n\xE0o: Trong C, h\xE0m th\u01B0 vi\u1EC7n chu\u1EA9\
  n `strlen()` th\u01B0\u1EDDng \u0111\u01B0\u1EE3c s\u1EED d\u1EE5ng \u0111\u1EC3\
  \ t\xECm \u0111\u1ED9 d\xE0i c\u1EE7a m\u1ED9t chu\u1ED7i. D\u01B0\u1EDBi \u0111\
  \xE2y l\xE0 m\u1ED9t v\xED d\u1EE5 nhanh."
lastmod: '2024-03-13T22:44:37.255371-06:00'
model: gpt-4-0125-preview
summary: "Trong C, h\xE0m th\u01B0 vi\u1EC7n chu\u1EA9n `strlen()` th\u01B0\u1EDD\
  ng \u0111\u01B0\u1EE3c s\u1EED d\u1EE5ng \u0111\u1EC3 t\xECm \u0111\u1ED9 d\xE0\
  i c\u1EE7a m\u1ED9t chu\u1ED7i."
title: "T\xECm ki\u1EBFm \u0111\u1ED9 d\xE0i c\u1EE7a m\u1ED9t chu\u1ED7i"
weight: 7
---

## Làm thế nào:
Trong C, hàm thư viện chuẩn `strlen()` thường được sử dụng để tìm độ dài của một chuỗi. Dưới đây là một ví dụ nhanh:

```c
#include <stdio.h>
#include <string.h>

int main() {
    char myString[] = "Hello, World!";
    size_t length = strlen(myString);
    
    printf("Độ dài của '%s' là %zu.\n", myString, length);
    
    return 0;
}
```

**Kết quả mẫu:**
```
Độ dài của 'Hello, World!' là 13.
```

Trong ví dụ này, `strlen()` nhận một chuỗi (`myString`) làm đầu vào và trả về độ dài của nó không bao gồm ký tự kết thúc NULL. Việc sử dụng `size_t` cho biến độ dài được khuyến khích bởi nó là một kiểu số nguyên không dấu, cho phép nó đại diện cho kích thước của đối tượng lớn nhất có thể có trên hệ thống.

## Đào sâu:
Hàm `strlen()` đã là một phần của thư viện chuẩn C từ khi ngôn ngữ này ra đời. Bên trong, nó hoạt động bằng cách tăng một bộ đếm khi nó duyệt qua chuỗi cho đến khi gặp ký tự kết thúc NULL. Sự đơn giản này, tuy nhiên, đi kèm với các xem xét về hiệu suất: bởi vì `strlen()` đếm các ký tự tại thời điểm chạy, việc gọi nó lặp đi lặp lại trên cùng một chuỗi trong một vòng lặp, ví dụ, là không hiệu quả.

Về mặt bảo mật, `strlen()` và các hàm xử lý chuỗi C khác không kiểm tra sự tràn bộ đệm một cách tự nhiên, làm cho việc lập trình cẩn thận trở nên thiết yếu để tránh những lỗ hổng. Các giải pháp thay thế hiện đại trong các ngôn ngữ khác, như các kiểu chuỗi bao gồm độ dài hoặc sử dụng xử lý bộ đệm an toàn mặc định, loại bỏ một số rủi ro và hiệu suất không hiệu quả này.

Mặc dù có những hạn chế, hiểu biết về `strlen()` và việc xử lý chuỗi thủ công trong C là rất quan trọng đối với các lập trình viên, đặc biệt khi làm việc với mã cấp thấp hoặc khi hiệu suất và kiểm soát bộ nhớ là tối quan trọng. Nó cũng mang lại cái nhìn sâu sắc về cách thức hoạt động của các trừu tượng hóa chuỗi cấp cao hơn trong các ngôn ngữ khác.
