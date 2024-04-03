---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 17:59:32.610147-07:00
description: "L\xE0m Th\u1EBF N\xE0o: Trong C, m\u1ED9t h\xE0m \u0111\u01B0\u1EE3\
  c khai b\xE1o v\u1EDBi ki\u1EC3u tr\u1EA3 v\u1EC1, t\xEAn, v\xE0 c\xE1c tham s\u1ED1\
  \ (n\u1EBFu c\xF3), theo sau l\xE0 m\u1ED9t kh\u1ED1i m\xE3 l\u1EC7nh. H\xE3y b\u1EAF\
  t \u0111\u1EA7u v\u1EDBi m\u1ED9t v\xED d\u1EE5 \u0111\u01A1n\u2026"
lastmod: '2024-03-13T22:44:37.275641-06:00'
model: gpt-4-0125-preview
summary: "Trong C, m\u1ED9t h\xE0m \u0111\u01B0\u1EE3c khai b\xE1o v\u1EDBi ki\u1EC3\
  u tr\u1EA3 v\u1EC1, t\xEAn, v\xE0 c\xE1c tham s\u1ED1 (n\u1EBFu c\xF3), theo sau\
  \ l\xE0 m\u1ED9t kh\u1ED1i m\xE3 l\u1EC7nh."
title: "S\u1EAFp x\u1EBFp m\xE3 l\u1EADp tr\xECnh v\xE0o trong h\xE0m"
weight: 18
---

## Làm Thế Nào:
Trong C, một hàm được khai báo với kiểu trả về, tên, và các tham số (nếu có), theo sau là một khối mã lệnh. Hãy bắt đầu với một ví dụ đơn giản: một hàm cộng hai số nguyên.

```c
#include <stdio.h>

// Khai báo hàm
int add(int a, int b);

int main() {
  int sum = add(5, 3);
  printf("Tổng là: %d\n", sum);
  return 0;
}

// Định nghĩa hàm
int add(int a, int b) {
  return a + b;
}
```

Kết quả:
```
Tổng là: 8
```

Bây giờ, hãy xem xét một ví dụ phức tạp hơn liên quan đến một kiểu dữ liệu do người dùng định nghĩa. Hàm này tính diện tích của một hình chữ nhật.

```c
#include <stdio.h>

// Định nghĩa cấu trúc cho một hình chữ nhật
typedef struct {
  int width;
  int height;
} Rectangle;

// Hàm tính diện tích hình chữ nhật
int calculateArea(Rectangle rect) {
  return rect.width * rect.height;
}

int main() {
  Rectangle myRect = {5, 10};
  int area = calculateArea(myRect);
  printf("Diện tích hình chữ nhật là: %d\n", area);
  return 0;
}
```

Kết quả:
```
Diện tích hình chữ nhật là: 50
```

## Sâu Hơn
Khái niệm về hàm trong C, thừa hưởng từ những thực hành lập trình trước đó, là cơ bản cho lập trình có cấu trúc. Hàm cho phép các lập trình viên tách rời chi tiết, quản lý độ phức tạp, và tổ chức mã lệnh của họ một cách logic. Kể từ khi ra đời, hàm đã trở thành một cấu trúc cốt lõi trong C, ảnh hưởng đến nhiều ngôn ngữ khác.

Tuy nhiên, khi các mô hình lập trình tiến hóa, các cách tiếp cận khác như lập trình hướng đối tượng (OOP) trong các ngôn ngữ như C++ và Java, đã mở rộng khái niệm của hàm với các phương thức liên quan đến đối tượng. Mặc dù C không hỗ trợ OOP ngay từ đầu, nhưng có khả năng mô phỏng thiết kế hướng đối tượng bằng cách cấu trúc hàm và dữ liệu một cách cẩn thận.

Trong lập trình hiện đại, hàm vẫn rất quan trọng, nhưng với sự tiến bộ trong tối ưu hóa bộ biên dịch và các tính năng ngôn ngữ, sự chú trọng có thể chuyển sang các hàm nội tuyến và mẫu trong C++ hoặc lambdas trong các ngôn ngữ như Python và JavaScript. Những điều này cung cấp sự linh hoạt hơn và thường là cú pháp ngắn gọn hơn để đạt được sự linh hoạt và khả năng tái sử dụng tương tự. Tuy nhiên, các nguyên tắc cơ bản học được thông qua việc tổ chức mã trong các hàm trong C là phổ quát áp dụng và tạo nên nền tảng của sự phát triển phần mềm hiệu quả và hiệu suất cao.
