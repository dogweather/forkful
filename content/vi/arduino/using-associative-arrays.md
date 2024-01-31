---
title:                "Sử dụng mảng liên kết"
date:                  2024-01-30T19:10:32.698227-07:00
model:                 gpt-4-0125-preview
simple_title:         "Sử dụng mảng liên kết"

category:             "Arduino"
tag:                  "Data Structures"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/arduino/using-associative-arrays.md"
changelog:
  - 2024-01-30, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cái gì & Tại sao?
Trong thế giới của Arduino, mảng kết hợp cho phép bạn ghép các key với các giá trị, kiểu như cách bạn ghép đôi tất với nhau. Chúng là lựa chọn tốt khi bạn cần lưu trữ và truy xuất dữ liệu sử dụng tên mô tả, giúp mã của bạn sạch sẽ và dễ hiểu hơn nhiều.

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
