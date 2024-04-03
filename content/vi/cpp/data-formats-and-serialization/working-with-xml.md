---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:11:46.771172-07:00
description: "L\xE0m vi\u1EC7c v\u1EDBi XML c\xF3 ngh\u0129a l\xE0 ph\xE2n t\xEDch\
  \ c\xFA ph\xE1p, t\u1EA1o v\xE0 thao t\xE1c d\u1EEF li\u1EC7u XML (eXtensible Markup\
  \ Language - Ng\xF4n ng\u1EEF \u0110\xE1nh d\u1EA5u M\u1EDF r\u1ED9ng). C\xE1c l\u1EAD\
  p tr\xECnh vi\xEAn\u2026"
lastmod: '2024-03-13T22:44:37.073071-06:00'
model: gpt-4-0125-preview
summary: "L\xE0m vi\u1EC7c v\u1EDBi XML c\xF3 ngh\u0129a l\xE0 ph\xE2n t\xEDch c\xFA\
  \ ph\xE1p, t\u1EA1o v\xE0 thao t\xE1c d\u1EEF li\u1EC7u XML (eXtensible Markup Language\
  \ - Ng\xF4n ng\u1EEF \u0110\xE1nh d\u1EA5u M\u1EDF r\u1ED9ng)."
title: "L\xE0m vi\u1EC7c v\u1EDBi XML"
weight: 40
---

## Gì và Tại Sao?
Làm việc với XML có nghĩa là phân tích cú pháp, tạo và thao tác dữ liệu XML (eXtensible Markup Language - Ngôn ngữ Đánh dấu Mở rộng). Các lập trình viên quản lý XML để xử lý trao đổi dữ liệu có cấu trúc, cấu hình và nhiều hơn nữa, do bản chất không phụ thuộc vào nền tảng của nó.

## Làm thế nào:
Dưới đây là một cách đơn giản để phân tích cú pháp XML sử dụng thư viện TinyXML-2:

```C++
#include <tinyxml2.h>
#include <iostream>

int main() {
    tinyxml2::XMLDocument doc;
    doc.Parse("<root><message>Xin chào, Thế giới!</message></root>");
    const char* noidung = doc.FirstChildElement("root")->FirstChildElement("message")->GetText();
    std::cout << noidung << std::endl;
    return 0;
}
```

Kết quả mẫu:

```
Xin chào, Thế giới!
```

Và đây là cách bạn tạo một tệp XML:

```C++
#include <tinyxml2.h>
#include <iostream>

int main() {
    tinyxml2::XMLDocument doc;
    auto* khai_bao = doc.NewDeclaration();
    doc.InsertFirstChild(khai_bao);
    auto* root = doc.NewElement("root");
    doc.InsertEndChild(root);
    auto* tin_nhan = doc.NewElement("message");
    tin_nhan->SetText("Xin chào, Thế giới!");
    root->InsertEndChild(tin_nhan);
    doc.SaveFile("output.xml");
    return 0;
}
```

Điều này tạo ra một tệp XML `output.xml` với nội dung:

```xml
<?xml version="1.0"?>
<root>
    <message>Xin chào, Thế giới!</message>
</root>
```

## Sâu hơn
XML đã trở nên quan trọng trong các dịch vụ web và lưu trữ dữ liệu kể từ cuối những năm 90. Mặc dù JSON và YAML hiện nay phổ biến hơn cho cấu hình và giao thức tương tác, XML vẫn rất quan trọng trong nhiều hệ thống doanh nghiệp. Phân tích cú pháp XML trong C++ có thể cảm thấy cũ kỹ với việc phân tích DOM/SAX thủ công. May mắn thay, các thư viện như TinyXML-2 đã đơn giản hoá nó. C++ không có hỗ trợ XML tích hợp; các thư viện như TinyXML-2, pugixml, hoặc Xerces giải quyết những khó khăn đó.

## Xem Thêm
- Tài liệu TinyXML-2: https://leethomason.github.io/tinyxml2/
- Thư viện pugixml: https://pugixml.org/
- Bộ phân tích cú pháp Xerces-C++: https://xerces.apache.org/xerces-c/
- Đặc tả XML của W3C: https://www.w3.org/XML/
