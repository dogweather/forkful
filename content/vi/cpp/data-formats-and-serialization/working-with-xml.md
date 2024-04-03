---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:11:46.771172-07:00
description: "L\xE0m th\u1EBF n\xE0o: D\u01B0\u1EDBi \u0111\xE2y l\xE0 m\u1ED9t c\xE1\
  ch \u0111\u01A1n gi\u1EA3n \u0111\u1EC3 ph\xE2n t\xEDch c\xFA ph\xE1p XML s\u1EED\
  \ d\u1EE5ng th\u01B0 vi\u1EC7n TinyXML-2."
lastmod: '2024-03-13T22:44:37.073071-06:00'
model: gpt-4-0125-preview
summary: "D\u01B0\u1EDBi \u0111\xE2y l\xE0 m\u1ED9t c\xE1ch \u0111\u01A1n gi\u1EA3\
  n \u0111\u1EC3 ph\xE2n t\xEDch c\xFA ph\xE1p XML s\u1EED d\u1EE5ng th\u01B0 vi\u1EC7\
  n TinyXML-2."
title: "L\xE0m vi\u1EC7c v\u1EDBi XML"
weight: 40
---

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
