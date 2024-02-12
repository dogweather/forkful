---
title:                "In đầu ra debug"
aliases:
- /vi/c/printing-debug-output.md
date:                  2024-02-03T18:06:32.670977-07:00
model:                 gpt-4-0125-preview
simple_title:         "In đầu ra debug"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/c/printing-debug-output.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Gì và Tại sao?

Việc in thông điệp gỡ lỗi là việc tạo ra những thông báo nhật ký tạm thời, mang tính thông tin giúp lập trình viên hiểu được dòng chảy và trạng thái của chương trình trong quá trình thực hiện. Lập trình viên thực hiện điều này để xác định và chẩn đoán các lỗi phần mềm hay hành vi không mong muốn trong lôgic của chương trình.

## Cách thực hiện:

Trong C, cách phổ biến nhất để in thông điệp gỡ lỗi là sử dụng hàm `printf` từ thư viện I/O chuẩn. Hàm `printf` cho phép định dạng đầu ra đến thiết bị đầu ra chuẩn, thường là màn hình. Dưới đây là một ví dụ đơn giản:

```c
#include <stdio.h>

int main() {
    int x = 5;
    printf("Gỡ lỗi: Giá trị của x là %d\n", x);
    
    // Lôgic chương trình của bạn ở đây
    
    return 0;
}
```

Đầu ra mẫu:

```
Gỡ lỗi: Giá trị của x là 5
```

Đối với việc in gỡ lỗi phức tạp hơn, bạn có thể muốn bao gồm thông tin về tên tệp và số dòng. Điều này có thể được thực hiện bằng cách sử dụng các macro định nghĩa trước `__FILE__` và `__LINE__` như sau:

```c
#define DEBUG_PRINT(fmt, args...) fprintf(stderr, "GỠ LỖI: %s:%d: " fmt, __FILE__, __LINE__, ##args)

int main() {
    int giatriKiemtra = 10;
    DEBUG_PRINT("Giá trị kiểm tra là %d\n", giatriKiemtra);
    
    // Lôgic chương trình của bạn ở đây
    
    return 0;
}
```

Đầu ra mẫu:

```
GỠ LỖI: example.c:6: Giá trị kiểm tra là 10
```

Lưu ý rằng trong ví dụ này, chúng ta đang sử dụng `fprintf` để xuất ra luồng lỗi chuẩn (`stderr`), thường phù hợp hơn cho các thông điệp gỡ lỗi.

## Sâu hơn nữa

Về mặt lịch sử, các kỹ thuật gỡ lỗi trong C đã được thực hiện một cách thủ công và cơ bản, do triết lý gần với phần cứng và tuổi đời của ngôn ngữ. Trong khi các ngôn ngữ hiện đại có thể bao gồm các thư viện gỡ lỗi tích hợp sẵn phức tạp hoặc phụ thuộc nhiều vào các tính năng của Môi trường Phát triển Tích hợp (IDE), các lập trình viên C thường tự chèn các câu lệnh in ra như được hiển thị ở trên để theo dõi việc thực thi chương trình của họ.

Một điều cần cảnh báo với các thông điệp gỡ lỗi là khả năng chúng làm rối loạn đầu ra và dẫn đến vấn đề về hiệu suất, đặc biệt là nếu chúng vô tình được để lại trong mã sản phẩm. Vì những lý do này, sử dụng biên dịch có điều kiện (ví dụ, `#ifdef DEBUG ... #endif`) có thể là cách tiếp cận tốt hơn, cho phép bao gồm hoặc loại bỏ các câu lệnh gỡ lỗi dựa trên các cờ biên dịch.

Hơn nữa, hiện nay có những công cụ và thư viện nâng cao hơn cho việc gỡ lỗi C, như GDB (GNU Debugger) và Valgrind cho việc phát hiện rò rỉ bộ nhớ. Những công cụ này cung cấp một cách tiếp cận tích hợp hơn cho việc gỡ lỗi, mà không cần phải sửa đổi mã bằng cách chèn các câu lệnh in.

Tuy nhiên, sự đơn giản và phản hồi tức thì của việc gỡ lỗi bằng `printf` không thể được đánh giá thấp, làm cho nó trở thành một công cụ hữu ích trong bộ công cụ của lập trình viên, đặc biệt là cho những ai mới học những điều tinh tế của C.
