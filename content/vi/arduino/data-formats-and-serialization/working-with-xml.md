---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:11:29.507750-07:00
description: "L\xFD do & M\u1EE5c \u0111\xEDch L\xE0m vi\u1EC7c v\u1EDBi XML tr\xEA\
  n Arduino bao g\u1ED3m vi\u1EC7c ph\xE2n t\xEDch v\xE0 x\u1EED l\xFD d\u1EEF li\u1EC7\
  u XML, th\u01B0\u1EDDng \u0111\u1EBFn t\u1EEB c\xE1c API web ho\u1EB7c c\xE1c file\
  \ c\u1EA5u h\xECnh. L\u1EADp tr\xECnh\u2026"
lastmod: '2024-04-05T22:37:45.730467-06:00'
model: gpt-4-0125-preview
summary: "L\xFD do & M\u1EE5c \u0111\xEDch L\xE0m vi\u1EC7c v\u1EDBi XML tr\xEAn Arduino\
  \ bao g\u1ED3m vi\u1EC7c ph\xE2n t\xEDch v\xE0 x\u1EED l\xFD d\u1EEF li\u1EC7u XML,\
  \ th\u01B0\u1EDDng \u0111\u1EBFn t\u1EEB c\xE1c API web ho\u1EB7c c\xE1c file c\u1EA5\
  u h\xECnh. L\u1EADp tr\xECnh vi\xEAn th\u1EF1c hi\u1EC7n \u0111i\u1EC1u n\xE0y \u0111\
  \u1EC3 t\xEDch h\u1EE3p v\u1EDBi c\xE1c d\u1ECBch v\u1EE5 s\u1EED d\u1EE5ng XML\
  \ cho vi\u1EC7c trao \u0111\u1ED5i d\u1EEF li\u1EC7u ho\u1EB7c \u0111\u1EC3 l\u01B0\
  u tr\u1EEF d\u1EEF li\u1EC7u theo m\u1ED9t \u0111\u1ECBnh d\u1EA1ng c\xF3 c\u1EA5\
  u tr\xFAc, d\u1EC5 \u0111\u1ECDc."
title: "L\xE0m vi\u1EC7c v\u1EDBi XML"
weight: 40
---

### Lý do & Mục đích
Làm việc với XML trên Arduino bao gồm việc phân tích và xử lý dữ liệu XML, thường đến từ các API web hoặc các file cấu hình. Lập trình viên thực hiện điều này để tích hợp với các dịch vụ sử dụng XML cho việc trao đổi dữ liệu hoặc để lưu trữ dữ liệu theo một định dạng có cấu trúc, dễ đọc.

### Cách thực hiện:
Chúng ta sẽ sử dụng thư viện `XMLWriter` để tạo XML và thư viện `tinyxml2` để phân tích nó. Trước hết, cài đặt các thư viện này thông qua Library Manager trong Arduino IDE của bạn.

Tạo một tài liệu XML:

```Arduino
#include <XMLWriter.h>

void setup() {
  Serial.begin(9600);
  
  XMLWriter xml(&Serial); // Sử dụng Serial để xuất ra
  
  xml.header();
  xml.tag("greeting").tag("text").text("Xin chào, thế giới!").close().close();
  xml.flush();
}

void loop() {
}
```

Giải mã một chuỗi XML:

```Arduino
#include <tinyxml2.h>

tinyxml2::XMLDocument doc;
doc.Parse("<greeting><text>Xin chào, thế giới!</text></greeting>");

tinyxml2::XMLElement* text = doc.FirstChildElement("greeting")->FirstChildElement("text");
if (text != nullptr) {
  Serial.println(text->GetText());
}
```

Mẫu xuất ra:

```
<greeting>
  <text>Xin chào, thế giới!</text>
</greeting>
```

### Sâu hơn nữa
XML, hay Ngôn ngữ Đánh dấu Mở rộng, là một ngôn ngữ đánh dấu định nghĩa một tập các quy tắc để mã hóa tài liệu trong một định dạng vừa dễ đọc vừa có thể xử lý bởi máy. XML đã xuất hiện từ cuối những năm 90 và được sử dụng rộng rãi trong các lĩnh vực khác nhau, đặc biệt là nơi cần trao đổi dữ liệu không phụ thuộc vào nền tảng. Nguyên cớ của việc những nguồn lực bộ nhớ hạn chế trên Arduino khiến việc làm việc với XML khó khăn hơn so với trên PC. Do đó, các thư viện nhẹ là rất quan trọng. Mặc dù JSON đã trở nên phổ biến hơn cho việc trao đổi dữ liệu nhờ cú pháp đơn giản hơn và kích thước nhỏ hơn, XML vẫn được sử dụng rộng rãi, đặc biệt là khi làm việc với các hệ thống cũ hoặc các ứng dụng đòi hỏi việc xác thực tài liệu qua các schema. Chìa khóa cho việc triển khai XML trên Arduino là việc phân tách luồng, đọc tài liệu theo từng phần để giữ cho việc sử dụng bộ nhớ ở mức thấp.

### Xem thêm
- [Tài liệu Thư viện TinyXML-2](https://leethomason.github.io/tinyxml2/)
- [Thư viện JSON Arduino](https://arduinojson.org/) cho một lựa chọn thay thế khi làm việc với dữ liệu JSON.
- [Học XML trên W3Schools](https://www.w3schools.com/xml/) để tìm hiểu chung về XML.
- [Tiêu chuẩn XML của W3C](https://www.w3.org/XML/) cho các tiêu chuẩn và khuyến nghị XML chính thức.
