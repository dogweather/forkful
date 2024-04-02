---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:59:52.090871-07:00
description: "Vi\u1EC7c tr\xEDch xu\u1EA5t c\xE1c chu\u1ED7i con c\xF3 ngh\u0129a\
  \ l\xE0 l\u1EA5y ra c\xE1c ph\u1EA7n c\u1EE5 th\u1EC3 t\u1EEB m\u1ED9t chu\u1ED7\
  i - gi\u1ED1ng nh\u01B0 l\u1EA5y m\u1ED9t l\xE1t b\xE1nh. L\u1EADp tr\xECnh vi\xEA\
  n th\u1EF1c hi\u1EC7n vi\u1EC7c n\xE0y \u0111\u1EC3 t\xE1ch d\u1EEF\u2026"
lastmod: '2024-03-13T22:44:36.976302-06:00'
model: gpt-4-0125-preview
summary: "Vi\u1EC7c tr\xEDch xu\u1EA5t c\xE1c chu\u1ED7i con c\xF3 ngh\u0129a l\xE0\
  \ l\u1EA5y ra c\xE1c ph\u1EA7n c\u1EE5 th\u1EC3 t\u1EEB m\u1ED9t chu\u1ED7i - gi\u1ED1\
  ng nh\u01B0 l\u1EA5y m\u1ED9t l\xE1t b\xE1nh. L\u1EADp tr\xECnh vi\xEAn th\u1EF1\
  c hi\u1EC7n vi\u1EC7c n\xE0y \u0111\u1EC3 t\xE1ch d\u1EEF\u2026"
title: "Tr\xEDch xu\u1EA5t chu\u1ED7i con"
weight: 6
---

## Cái gì & Tại sao?

Việc trích xuất các chuỗi con có nghĩa là lấy ra các phần cụ thể từ một chuỗi - giống như lấy một lát bánh. Lập trình viên thực hiện việc này để tách dữ liệu, làm sạch đầu vào, hoặc thao tác với văn bản cho các công việc như phân tích tin nhắn từ cảm biến.

## Cách thực hiện:

Các chuỗi trong Arduino có thể được cắt và chia nhỏ sử dụng `substring()`:

```arduino
void setup() {
  Serial.begin(9600);
  String phrase = "Hello, Arduino World!";
  String greeting = phrase.substring(0, 5);
  String location = phrase.substring(7, 19);
  
  Serial.println(greeting); // In ra "Hello"
  Serial.println(location); // In ra "Arduino World"
}

void loop() {
  // Không có gì để lặp lại ở đây.
}
```

Đầu ra trên Serial Monitor:
```
Hello
Arduino World
```

## Sâu hơn

Trước khi Arduino làm cho việc này trở nên đơn giản, các lập trình viên sử dụng mảng char và các hàm như `strncpy` trong C. Chúng không chỉ là di sản lịch sử, mà vẫn còn được sử dụng cho các hoạt động cấp thấp. Hàm `substring()` trong Arduino thực chất là một lớp bọc giúp chúng ta dễ dàng hơn khi xử lý với các đối tượng String. Nhưng hãy lưu ý, việc sử dụng `String` có thể dẫn đến việc phân mảnh bộ nhớ. Nếu sự ổn định là rất quan trọng, đặc biệt trong các chương trình chạy dài hoặc phức tạp, hãy cân nhắc về phương pháp truyền thống sử dụng mảng `char`.

Các phương án thay thế cho `substring()` bao gồm việc điều chỉnh mảng char trực tiếp hoặc sử dụng các hàm như `strtok()`. Những phương án này có thể hiệu quả hơn nhưng có thể khiến bạn phải quản lý nhiều mã hơn.

Bên trong, hàm `substring()` tạo một đối tượng String mới chứa các ký tự từ chỉ số bắt đầu đến ngay trước chỉ số kết thúc, có thể bỏ qua nếu bạn muốn lấy mọi thứ cho đến cuối.

## Xem thêm:

- Tham khảo chuỗi Arduino: https://www.arduino.cc/reference/en/language/variables/data-types/stringobject/
- Quản lý bộ nhớ trong Arduino: https://learn.arduino.cc/programming/variables-and-data-types/memory-management
- Phương thức substr của C++ `std::string`, để so sánh: http://www.cplusplus.com/reference/string/string/substr/
