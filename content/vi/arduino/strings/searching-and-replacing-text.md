---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:07:34.890695-07:00
description: "L\xE0m Th\u1EBF N\xE0o: Arduino kh\xF4ng h\u1ED7 tr\u1EE3 t\xECm ki\u1EBF\
  m v\xE0 thay th\u1EBF chu\u1ED7i m\u1ED9t c\xE1ch t\u1EF1 nhi\xEAn nh\u01B0 c\xE1\
  c ng\xF4n ng\u1EEF c\u1EA5p cao. Tuy nhi\xEAn, b\u1EA1n c\xF3 th\u1EC3 l\xE0m vi\u1EC7\
  c v\u1EDBi m\u1EA3ng k\xFD t\u1EF1\u2026"
lastmod: '2024-03-13T22:44:36.971193-06:00'
model: gpt-4-0125-preview
summary: "Arduino kh\xF4ng h\u1ED7 tr\u1EE3 t\xECm ki\u1EBFm v\xE0 thay th\u1EBF chu\u1ED7\
  i m\u1ED9t c\xE1ch t\u1EF1 nhi\xEAn nh\u01B0 c\xE1c ng\xF4n ng\u1EEF c\u1EA5p cao."
title: "T\xECm ki\u1EBFm v\xE0 thay th\u1EBF v\u0103n b\u1EA3n"
weight: 10
---

## Làm Thế Nào:
Arduino không hỗ trợ tìm kiếm và thay thế chuỗi một cách tự nhiên như các ngôn ngữ cấp cao. Tuy nhiên, bạn có thể làm việc với mảng ký tự hoặc sử dụng lớp `String` cung cấp phương thức `replace()`. Mặc dù cách đầu tiên tiết kiệm bộ nhớ hơn, nhưng cách sau dễ dàng hơn. Hãy tập trung vào lớp `String` để rõ ràng hơn.

```Arduino
void setup() {
  Serial.begin(9600);
  String text = "I like apples and apples are great!";
  text.replace("apples", "oranges");
  Serial.println(text);
}

void loop() {
  // Không có gì để làm ở đây.
}
```

Kết Quả Mẫu:
```
I like oranges and oranges are great!
```

## Sâu Hơn
Ngày xưa, việc thao tác chuỗi trên vi điều khiển là hiếm — bộ nhớ bị hạn chế và ứng dụng đơn giản hơn. Ngày nay, với các dự án phức tạp hơn và không gian bộ nhớ dồi dào (nhờ tiến bộ trong công nghệ vi điều khiển), những tiện ích như vậy khá phổ biến.

Nếu bạn không muốn sử dụng lớp `String` vì nó sử dụng bộ nhớ động, có thể gây ra mảnh vỡ, bạn vẫn có thể tìm kiếm và thay thế trong chuỗi kiểu C (mảng ký tự kết thúc bằng null) bằng cách sử dụng các hàm như `strchr()`, `strstr()`, và sao chép hoặc thay thế thủ công bằng vòng lặp. Cách này đòi hỏi nhiều công việc hơn nhưng cho bạn kiểm soát bộ nhớ tốt hơn.

Ví dụ, một cách thay thế một chuỗi con có thể như sau:

```Arduino
void replaceSubstring(char *input, const char *search, const char *replace) {
  char buffer[100];
  char *p;

  // 'strstr' kiểm tra xem 'search' có là một phần của 'input' không.
  if (!(p = strstr(input, search))) return;

  // Sao chép cho đến điểm mà 'search' được tìm thấy.
  strncpy(buffer, input, p - input);
  buffer[p - input] = '\0';

  // Thêm 'replace' và phần còn lại của 'input' sau 'search'.
  sprintf(buffer+(p - input), "%s%s", replace, p + strlen(search));

  // Xuất kết quả
  strcpy(input, buffer);
}

void setup() {
  Serial.begin(9600);
  char text[] = "I like apples and apples are great!";
  replaceSubstring(text, "apples", "oranges");
  Serial.println(text);
}

void loop() {
  // Vẫn không có gì để làm ở đây.
}
```

Kết Quả Mẫu:
```
I like oranges and oranges are great!
```

## Xem Thêm
- [Arduino Reference: Đối Tượng String](https://www.arduino.cc/reference/en/language/variables/data-types/stringobject/)
- [Arduino Reference: Hàm Thay Thế String](https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/replace/)
- [Cplusplus.com: Các Hàm Chuỗi C](http://www.cplusplus.com/reference/cstring/)
