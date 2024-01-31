---
title:                "Tái cấu trúc mã"
date:                  2024-01-28T22:06:53.942970-07:00
model:                 gpt-4-0125-preview
simple_title:         "Tái cấu trúc mã"

category:             "C"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/c/refactoring.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cái Gì & Tại Sao?
Tái cấu trúc là quá trình cấu trúc lại mã máy tính hiện có mà không thay đổi hành vi bên ngoài của nó. Lập trình viên thực hiện điều này để cải thiện khả năng đọc, giảm độ phức tạp, hoặc làm cho mã dễ bảo trì và mở rộng hơn, có thể tiết kiệm được một khoản thời gian và giảm đau đầu về sau.

## Làm Thế Nào:
Hãy làm mới một số mã. Hãy tưởng tượng bạn có một hàm tính trung bình của các số nguyên trong một mảng. Ngay từ cái nhìn đầu tiên, nó khá là rối rắm.

**Trước Khi Tái Cấu Trúc:**
```C
#include <stdio.h>

double calculateStuff(int numbers[], int size) {
    int sum = 0;
    double average;
    for(int i = 0; i < size; sum += numbers[i++]); // Tính tổng trong điều kiện vòng lặp for, ouch!
    average = (double)sum / size;
    return average;
}

int main() {
    int array[] = {23, 42, 57, 16, 5, 102};
    int length = sizeof(array) / sizeof(array[0]);
    printf("Trung bình: %f\n", calculateStuff(array, length));

    return 0;
}
```

**Sau Khi Tái Cấu Trúc:**
```C
#include <stdio.h>

int calculateSum(const int numbers[], int size) {
    int sum = 0;
    for (int i = 0; i < size; ++i) {
        sum += numbers[i];
    }
    return sum;
}

double calculateAverage(const int numbers[], int size) {
    int sum = calculateSum(numbers, size);
    return (double)sum / size;
}

int main() {
    int array[] = {23, 42, 57, 16, 5, 102};
    int length = sizeof(array) / sizeof(array[0]);
    printf("Trung bình: %f\n", calculateAverage(array, length));
    return 0;
}
```
Ngay cả với ví dụ đơn giản này, bạn có thể thấy cách tách hàm khiến mã sạch sẽ và dễ bảo trì hơn. Mỗi hàm bây giờ đều có một trách nhiệm riêng – một nguyên tắc chính trong lập trình sạch.

## Sâu Hơn
Thuật ngữ "tái cấu trúc" được phổ biến vào cuối những năm 90, đặc biệt với sự xuất bản của cuốn sách "Refactoring: Improving the Design of Existing Code" của Martin Fowler. Tái cấu trúc không ngụ ý sửa chữa lỗi hay thêm tính năng mới, mà là về việc cải thiện cấu trúc của mã.

Có nhiều công cụ tái cấu trúc tinh tế và IDE (Môi trường Phát triển Tích hợp) giúp tự động hóa quá trình này, như CLion cho C và C++, nhưng việc hiểu những gì đang diễn ra ở phía dưới là rất quan trọng.

Các phương án thay thế cho việc tái cấu trúc có thể bao gồm viết lại mã từ đầu (rủi ro và thường không cần thiết) hoặc chấp nhận nợ kỹ thuật (có thể tốn kém hơn về lâu dài). Chi tiết thực hiện có thể thay đổi tùy theo dự án, nhưng các tái cấu trúc phổ biến bao gồm đổi tên biến cho rõ ràng, chia nhỏ hàm lớn thành nhiều hàm nhỏ hơn, và thay thế số ma thuật bằng hằng số có tên.

Ngoài ra, các mô hình như DRY (Don't Repeat Yourself - Đừng Lặp Lại Chính Mình) và nguyên tắc SOLID có thể hướng dẫn hành trình tái cấu trúc của bạn, hướng tới một cơ sở mã dễ kiểm tra, hiểu và hợp tác hơn.

## Xem Thêm
Để đi sâu hơn vào biển của tái cấu trúc, hãy xem qua:

- Trang chủ của Martin Fowler: https://martinfowler.com/ với kho báu các bài viết và tài nguyên về tái cấu trúc và thiết kế phần mềm.
- Refactoring.com: https://refactoring.com/ cung cấp các ví dụ và danh mục kỹ thuật tái cấu trúc.
- Cuốn sách "Refactoring": Được coi là kinh thánh của tái cấu trúc, đọc nó cho bạn cái nhìn toàn diện về phương pháp.
- "Clean Code: A Handbook of Agile Software Craftsmanship" của Robert C. Martin, nói về việc viết mã dễ hiểu và dễ bảo trì.
