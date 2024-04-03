---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:56:17.622860-07:00
description: "Bi\u1EBFn m\u1ED7i k\xFD t\u1EF1 c\u1EE7a m\u1ED9t chu\u1ED7i th\xE0\
  nh ch\u1EEF hoa c\xF3 ngh\u0129a l\xE0 l\xE0m cho m\u1ECDi k\xFD t\u1EF1 \u0111\u1EC1\
  u tr\u1EDF th\xE0nh ch\u1EEF in hoa. C\xE1c l\u1EADp tr\xECnh vi\xEAn l\xE0m \u0111\
  i\u1EC1u n\xE0y \u0111\u1EC3 \u0111\u1EA1t \u0111\u01B0\u1EE3c s\u1EF1 nh\u1EA5\
  t\u2026"
lastmod: '2024-03-13T22:44:36.968583-06:00'
model: gpt-4-0125-preview
summary: "Bi\u1EBFn m\u1ED7i k\xFD t\u1EF1 c\u1EE7a m\u1ED9t chu\u1ED7i th\xE0nh ch\u1EEF\
  \ hoa c\xF3 ngh\u0129a l\xE0 l\xE0m cho m\u1ECDi k\xFD t\u1EF1 \u0111\u1EC1u tr\u1EDF\
  \ th\xE0nh ch\u1EEF in hoa."
title: "Vi\u1EBFt hoa m\u1ED9t chu\u1ED7i"
weight: 2
---

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
