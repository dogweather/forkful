---
title:                "Sắp xếp mã thành các hàm"
date:                  2024-01-28T22:03:18.360320-07:00
model:                 gpt-4-0125-preview
simple_title:         "Sắp xếp mã thành các hàm"
programming_language: "C"
category:             "C"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/c/organizing-code-into-functions.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cái gì và Tại sao?
Việc tổ chức mã thành các hàm là về việc phân chia mã thành các khối có thể tái sử dụng thực hiện các nhiệm vụ cụ thể. Điều này làm cho mã dễ đọc, dễ gỡ lỗi và dễ bảo trì hơn.

## Làm thế nào:
Hãy lấy một ví dụ đơn giản: giả sử, bạn muốn cộng hai số nhiều lần.

Không sử dụng hàm:
```C
#include <stdio.h>

int main() {
    int sum1 = 5 + 3;
    printf("Tổng1: %d\n", sum1);
    
    int sum2 = 2 + 8;
    printf("Tổng2: %d\n", sum2);
    
    // Thêm các phép cộng ở đây...
    
    return 0;
}
```

Sử dụng hàm:
```C
#include <stdio.h>

int add(int a, int b) {
    return a + b;
}

int main() {
    int sum1 = add(5, 3);
    printf("Tổng1: %d\n", sum1);
    
    int sum2 = add(2, 8);
    printf("Tổng2: %d\n", sum2);
    
    // Sử dụng hàm add() cho những phép cộng thêm...
    
    return 0;
}
```

Kết quả đầu ra:
```
Tổng1: 8
Tổng2: 10
```

## Sâu hơn
Trước khi C có hàm, việc lập trình thường được thực hiện một cách tuyến tính, giống như một công thức. Nhưng khi các chương trình phát triển, sự trùng lặp mã trở thành một vấn đề. Hàm là giải pháp - chúng cho phép chúng ta thực thi cùng một khối mã từ các phần khác nhau của chương trình mà không cần viết lại mỗi lần. Điều này không chỉ tiết kiệm không gian mà còn tiết kiệm thời gian khi thực hiện cập nhật: thay đổi hàm ở một nơi và mỗi phần của mã của bạn sử dụng nó đều được cập nhật.

Các lựa chọn thay thế cho hàm có thể bao gồm mã nội dòng, macro, hoặc việc sao chép và dán mã, nhưng những cái này có thể dẫn đến mã phình to, dễ phạm lỗi và khó bảo trì. Ngược lại, hàm đóng gói chức năng, định rõ giao diện và có thể giảm tác dụng phụ với việc sử dụng phạm vi thích hợp.

Khi bạn triển khai hàm, cần xem xét một vài chi tiết: một là, hãy cố gắng làm cho chúng chỉ thực hiện một nhiệm vụ - đây được biết đến là Nguyên tắc Trách nhiệm Đơn lẻ. Hai, tên có ý nghĩa - chọn tên mô tả cho hàm và các tham số của chúng để làm cho mã của bạn tự tài liệu hóa.

## Xem thêm
Để biết thêm về hàm trong C, hãy tham khảo những tài liệu sau:

- Tham khảo Thư viện Tiêu chuẩn C: https://en.cppreference.com/w/c/header
- Lập trình C: Một cách tiếp cận hiện đại bởi K.N. King: Một cuốn sách với cái nhìn sâu sắc về hàm.
- Learn-C.org: Mục hàm: https://www.learn-c.org/en/Functions
