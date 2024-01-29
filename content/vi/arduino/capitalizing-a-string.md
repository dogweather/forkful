---
title:                "Viết hoa một chuỗi"
date:                  2024-01-28T21:56:17.622860-07:00
model:                 gpt-4-0125-preview
simple_title:         "Viết hoa một chuỗi"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/arduino/capitalizing-a-string.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cái gì & Tại sao?

Biến mỗi ký tự của một chuỗi thành chữ hoa có nghĩa là làm cho mọi ký tự đều trở thành chữ in hoa. Các lập trình viên làm điều này để đạt được sự nhất quán, đặc biệt là trong giao diện người dùng hoặc khi chuẩn bị dữ liệu cho việc lưu trữ hoặc so sánh.

## Cách thực hiện:

Trong môi trường Arduino, không có hàm được xây dựng sẵn nào để biến toàn bộ một chuỗi thành chữ hoa, vì vậy chúng ta sẽ viết một hàm đơn giản để thực hiện việc này:

```Arduino
void setup() {
  Serial.begin(9600);
  char example[] = "hello, world!";
  chuyenChuoiThanhChuHoa(example);
  Serial.println(example);
}

void loop() {
  // Không cần làm gì ở đây
}

void chuyenChuoiThanhChuHoa(char* str) {
  for (int i = 0; str[i] != '\0'; i++) {
    str[i] = toupper((unsigned char)str[i]);
  }
}
```

Sau khi chạy sketch, đầu ra trên màn hình serial hiển thị:
```
HELLO, WORLD!
```

## Tìm hiểu sâu hơn

Trong lịch sử, việc thao tác chuỗi trong những ngôn ngữ cấp thấp như C đòi hỏi phải xử lý từng ký tự riêng lẻ do thiếu các hàm thao tác chuỗi cấp cao. Truyền thống này được kế thừa qua các biến thể C++ của Arduino.

Một số phương án thay thế bao gồm việc sử dụng các đối tượng `String` có sẵn trong C++ của Arduino và gọi phương thức `.toUpperCase()`. Tuy nhiên, điều này sẽ tiêu tốn nhiều bộ nhớ hơn. Đối với những môi trường bị hạn chế về bộ nhớ như vi điều khiển, thường tốt hơn khi làm việc với mảng ký tự kiểu C (chuỗi) và thao tác trực tiếp trên đó.

Chi tiết thực hiện cần nhớ khi biến chuỗi thành chữ hoa trong Arduino:
- Đảm bảo chuỗi có thể thay đổi (tức là, một mảng ký tự).
- Sử dụng hàm `toupper` từ `<ctype.h>` để chuyển đổi từng ký tự.
- Thao tác chuỗi có thể dẫn đến các vấn đề về bộ nhớ như tràn bộ đệm nếu không được xử lý cẩn thận.

## Xem thêm

- Tham khảo Arduino cho phương thức String `.toUpperCase()`: https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/touppercase/
- Tham khảo `toupper` tại Cplusplus.com: http://www.cplusplus.com/reference/cctype/toupper/ 
- Ví dụ về thao tác chuỗi Arduino: https://www.arduino.cc/en/Tutorial/BuiltInExamples/StringAdditionOperator
