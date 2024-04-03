---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:58:44.769792-07:00
description: "C\xE1ch th\u1EF1c hi\u1EC7n: Gi\u1EA3 s\u1EED ch\xFAng ta mu\u1ED1n\
  \ lo\u1EA1i b\u1ECF t\u1EA5t c\u1EA3 c\xE1c ch\u1EEF s\u1ED1 s\u1ED1 h\u1ECDc kh\u1ECF\
  i chu\u1ED7i c\u1EE7a ch\xFAng ta. Ch\xFAng ta c\xF3 m\u1ED9t chu\u1ED7i v\u1EDB\
  i m\u1ED9t s\u1ED1 ch\u1EEF s\u1ED1 ng\u1EABu nhi\xEAn, v\xE0\u2026"
lastmod: '2024-03-13T22:44:36.969901-06:00'
model: gpt-4-0125-preview
summary: "Gi\u1EA3 s\u1EED ch\xFAng ta mu\u1ED1n lo\u1EA1i b\u1ECF t\u1EA5t c\u1EA3\
  \ c\xE1c ch\u1EEF s\u1ED1 s\u1ED1 h\u1ECDc kh\u1ECFi chu\u1ED7i c\u1EE7a ch\xFA\
  ng ta."
title: "X\xF3a c\xE1c k\xFD t\u1EF1 ph\xF9 h\u1EE3p v\u1EDBi m\u1ED9t m\u1EABu"
weight: 5
---

## Cách thực hiện:
Giả sử chúng ta muốn loại bỏ tất cả các chữ số số học khỏi chuỗi của chúng ta. Chúng ta có một chuỗi với một số chữ số ngẫu nhiên, và chúng ta đang hướng tới kết quả chỉ gồm chữ cái.

```Arduino
void setup() {
  Serial.begin(9600);

  // Chuỗi ban đầu của chúng ta có chứa số
  String stringWithNumbers = "Ar3du1n0 1s aw3som3!";
  String cleanedString = deletePattern(stringWithNumbers, "0123456789");

  // In chuỗi đã được làm sạch
  Serial.println(cleanedString);
}

void loop() {
  // Không có gì để thực hiện ở đây
}

String deletePattern(String str, String pattern) {
  for (unsigned int i = 0; i < pattern.length(); i++) {
    str.replace(String(pattern[i]), "");
  }
  return str;
}
```

Nếu bạn tải và chạy điều này trên Arduino của mình, bạn sẽ thấy chuỗi không có số trong màn hình serial:

```
Arduino is awesome!
```

## Sâu hơn nữa
Việc loại bỏ ký tự khớp với một mẫu cụ thể không phải là một khái niệm mới. Các ngôn ngữ lập trình sớm đã có các hàm để xử lý và thao tác chuỗi. Trong Arduino, mặc dù không tồn tại một hàm cấp cao cho việc xóa mẫu một cách tự nhiên, chúng ta có thể tạo ra lô-gic tùy chỉnh của mình, như trong hàm `deletePattern` ở trên.

Có những lựa chọn thay thế trong các ngôn ngữ khác, như regex (biểu thức chính quy) trong Python hoặc JavaScript, nhưng môi trường lập trình của Arduino cơ bản hơn. Nó không bao gồm các hàm regex ngay từ đầu, chủ yếu do sức mạnh xử lý và bộ nhớ giới hạn của nó.

Bên dưới lớp vỏ, hàm `deletePattern` của chúng ta lặp qua chuỗi mẫu của chúng ta, sử dụng phương thức `String.replace()` để tìm kiếm ký tự hiện tại, và thay thế nó bằng một chuỗi trống, do đó "xóa" nó khỏi chuỗi gốc của chúng ta.

## Xem thêm
- Thao tác chuỗi với Arduino: https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/
- Tham khảo String Arduino: https://www.arduino.cc/reference/en/language/variables/data-types/stringobject/
- Thêm về thay thế chuỗi: http://www.cplusplus.com/reference/string/string/replace/
