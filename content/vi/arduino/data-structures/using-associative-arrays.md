---
changelog:
- 2024-01-30, gpt-4-0125-preview, translated from English
date: 2024-01-30 19:10:32.698227-07:00
description: "C\xE1ch th\u1EF1c hi\u1EC7n: N\xF3i m\u1ED9t c\xE1ch nghi\xEAm ng\u1EB7\
  t, Arduino kh\xF4ng h\u1ED7 tr\u1EE3 s\u1EB5n t\xEDnh n\u0103ng m\u1EA3ng k\u1EBF\
  t h\u1EE3p nh\u01B0 b\u1EA1n th\u1EA5y trong c\xE1c ng\xF4n ng\u1EEF c\u1EA5p cao.\
  \ Nh\u01B0ng, \u0111\u1EEBng lo. Ch\xFAng\u2026"
lastmod: '2024-03-13T22:44:36.981317-06:00'
model: gpt-4-0125-preview
summary: "N\xF3i m\u1ED9t c\xE1ch nghi\xEAm ng\u1EB7t, Arduino kh\xF4ng h\u1ED7 tr\u1EE3\
  \ s\u1EB5n t\xEDnh n\u0103ng m\u1EA3ng k\u1EBFt h\u1EE3p nh\u01B0 b\u1EA1n th\u1EA5\
  y trong c\xE1c ng\xF4n ng\u1EEF c\u1EA5p cao."
title: "S\u1EED d\u1EE5ng m\u1EA3ng li\xEAn k\u1EBFt"
weight: 15
---

## Cách thực hiện:
Nói một cách nghiêm ngặt, Arduino không hỗ trợ sẵn tính năng mảng kết hợp như bạn thấy trong các ngôn ngữ cấp cao. Nhưng, đừng lo. Chúng ta có thể trở nên sáng tạo sử dụng cấu trúc và mảng để mô phỏng chức năng này. Dưới đây là một ví dụ đơn giản để tạo một "mảng kết hợp" cơ bản để lưu trữ và truy cập nhiệt độ cho các thành phố khác nhau.

Đầu tiên, định nghĩa một cấu trúc để giữ thành phố (key) và nhiệt độ của nó (giá trị):

```cpp
struct CityTemperature {
  String city;
  float temperature;
};
```

Tiếp theo, khởi tạo một mảng các đối tượng `CityTemperature`:

```cpp
CityTemperature temperatures[] = {
  {"New York", 19.5},
  {"Los Angeles", 22.0},
  {"Chicago", 17.0}
};
```

Dưới đây là cách bạn có thể truy cập và hiển thị nhiệt độ của một thành phố cụ thể:

```cpp
void setup() {
  Serial.begin(9600);
  for(int i = 0; i < 3; i++) {
    if(temperatures[i].city == "Los Angeles") {
      Serial.print("Nhiệt độ ở Los Angeles là: ");
      Serial.println(temperatures[i].temperature);
    }
  }
}

void loop() {
  // Không có gì ở đây cho đến bây giờ.
}
```

Chạy mã này sẽ cho bạn kết quả:

```
Nhiệt độ ở Los Angeles là: 22.0
```

## Sâu hơn
Trước đây, các ngôn ngữ lập trình như C và C++ (từ đó cú pháp Arduino được phái sinh) không đi kèm với mảng kết hợp có sẵn, dẫn đến các giải pháp tạm thời như ví dụ trên. Cách tiếp cận này tương đối đơn giản nhưng không mở rộng tốt khi kích thước dữ liệu tăng lên do thời gian tìm kiếm của nó là O(n).

Các ngôn ngữ như Python cung cấp từ điển, và JavaScript có đối tượng cho mục đích này, cả hai đều hiệu quả hơn nhiều trong việc quản lý cặp key-value. Trong Arduino, khi hiệu suất và hiệu quả trở nên quan trọng, các nhà phát triển có thể chọn các cấu trúc dữ liệu chuyên biệt hơn, như bảng băm, được triển khai thông qua thư viện.

Mặc dù Arduino không hỗ trợ tự nhiên mảng kết hợp, cộng đồng đã phát triển các thư viện như `HashMap` có thể được thêm vào dự án của bạn để cung cấp chức năng tương tự với hiệu suất tốt hơn so với cách tiếp cận tự làm. Những thư viện này thường cung cấp một phương tiện quản lý mảng kết hợp một cách thanh lịch và hiệu quả hơn, đặc biệt là cho các dự án phức tạp hơn.
