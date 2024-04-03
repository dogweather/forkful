---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 17:58:37.283052-07:00
description: "Vi\u1EC7c x\u1EED l\xFD l\u1ED7i trong C bao g\u1ED3m vi\u1EC7c ph\xE1\
  t hi\u1EC7n v\xE0 ph\u1EA3n \u1EE9ng v\u1EDBi c\xE1c \u0111i\u1EC1u ki\u1EC7n b\u1EA5\
  t th\u01B0\u1EDDng ph\xE1t sinh trong qu\xE1 tr\xECnh th\u1EF1c thi ch\u01B0\u01A1\
  ng tr\xECnh. L\u1EADp tr\xECnh vi\xEAn l\xE0m\u2026"
lastmod: '2024-03-13T22:44:37.278229-06:00'
model: gpt-4-0125-preview
summary: "Vi\u1EC7c x\u1EED l\xFD l\u1ED7i trong C bao g\u1ED3m vi\u1EC7c ph\xE1t\
  \ hi\u1EC7n v\xE0 ph\u1EA3n \u1EE9ng v\u1EDBi c\xE1c \u0111i\u1EC1u ki\u1EC7n b\u1EA5\
  t th\u01B0\u1EDDng ph\xE1t sinh trong qu\xE1 tr\xECnh th\u1EF1c thi ch\u01B0\u01A1\
  ng tr\xECnh."
title: "X\u1EED l\xFD l\u1ED7i"
weight: 16
---

## Cách thức:
C không có hỗ trợ tích hợp sẵn cho ngoại lệ như một số ngôn ngữ khác. Thay vào đó, nó dựa vào một vài chiến lược xử lý lỗi thông thường, như trả về các giá trị đặc biệt từ các hàm và thiết lập các biến toàn cục như `errno`.

**Trả Về Các Giá Trị Đặc Biệt**

Các hàm có thể chỉ ra lỗi bằng cách trả về một giá trị cụ thể không có khả năng là một kết quả hợp lệ. Dưới đây là một ví dụ với số nguyên:

```c
#include <stdio.h>

int inverse(int number, double *result) {
    if (number == 0) {
        return -1; // Trường hợp lỗi
    } else {
        *result = 1.0 / number;
        return 0; // Thành công
    }
}

int main() {
    double result;
    if (inverse(0, &result) < 0) {
        printf("Lỗi: Chia cho số không.\n");
    } else {
        printf("Nghịch đảo là: %f\n", result);
    }
    
    return 0;
}
```

**Đầu ra:**
```
Lỗi: Chia cho số không.
```

**Kiểm Tra `errno`**

Đối với các hàm thư viện, đặc biệt là những hàm tương tác với hệ thống hoặc HĐH (như I/O file), `errno` được thiết lập khi một lỗi xảy ra. Để sử dụng nó, bao gồm `errno.h` và kiểm tra `errno` sau một sự cố nghi ngờ:

```c
#include <stdio.h>
#include <errno.h>
#include <string.h>

int main() {
    FILE *file = fopen("nonexistent.txt", "r");
    if (file == NULL) {
        printf("Lỗi mở file: %s\n", strerror(errno));
    }
    
    return 0;
}
```

**Đầu ra:**
```
Lỗi mở file: Không tìm thấy file hoặc thư mục
```

## Sâu Hơn
Lịch sử, ngôn ngữ lập trình C với thiết kế tối giản đã loại trừ một cơ chế xử lý ngoại lệ tích hợp sẵn, phản ánh nguồn gốc lập trình hệ thống cấp thấp của nó, nơi hiệu suất tối đa và kiểm soát sát sao tới phần cứng là quan trọng. Thay vào đó, C áp dụng một cách tiếp cận xử lý lỗi thủ công phù hợp với triết lý cung cấp cho lập trình viên càng nhiều quyền kiểm soát càng tốt, ngay cả với chi phí của sự tiện lợi.

Mặc dù cách tiếp cận này phù hợp với mục tiêu thiết kế của C, nhưng cũng có thể dẫn đến mã kiểm tra lỗi dài dòng và khả năng bỏ lỡ kiểm tra lỗi, điều mà các ngôn ngữ hiện đại giải quyết với cơ chế xử lý ngoại lệ có cấu trúc. Ví dụ, các ngoại lệ trong ngôn ngữ như Java hay C# cho phép xử lý lỗi tập trung, làm cho mã sạch sẽ hơn và quản lý lỗi dễ dàng hơn. Tuy nhiên, các ngoại lệ giới thiệu chi phí và phức tạp của chúng, có thể không lý tưởng cho lập trình cấp hệ thống nơi C tỏa sáng.

Mặc dù có vẻ thô sơ, cách xử lý lỗi thủ công trong C đã ảnh hưởng đến thiết kế quản lý lỗi trong nhiều ngôn ngữ khác, cung cấp một mô hình nơi sự rõ ràng của điều kiện lỗi có thể dẫn đến mã dễ dàng dự đoán và gỡ lỗi hơn. Đối với các hệ thống quan trọng, nơi các sự cố cần được quản lý một cách nhẹ nhàng, cách tiếp cận xử lý lỗi của C - kết hợp với các phương pháp hay nhất hiện đại như thư viện và quy ước xử lý lỗi - đảm bảo độ robustness và đáng tin cậy.
