---
title:                "Làm việc với XML"
aliases:
- /vi/cpp/working-with-xml/
date:                  2024-01-28T22:11:46.771172-07:00
model:                 gpt-4-0125-preview
simple_title:         "Làm việc với XML"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/cpp/working-with-xml.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

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
