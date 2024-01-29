---
title:                "Làm việc với XML"
date:                  2024-01-28T22:12:10.855804-07:00
model:                 gpt-4-0125-preview
simple_title:         "Làm việc với XML"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/gleam/working-with-xml.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Gì & Tại Sao?
Làm việc với XML bao gồm việc phân tích cú pháp, thao tác và tạo ra các tài liệu XML, được sử dụng cho trao đổi dữ liệu do định dạng cấu trúc và phổ biến của chúng. Các lập trình viên xử lý XML để giao tiếp với vô số hệ thống nơi XML là ngôn ngữ chung của dữ liệu.

## Làm Thế Nào:
Gleam không hỗ trợ XML một cách tự nhiên, vì vậy chúng ta sẽ sử dụng một thư viện bên ngoài như `gleam_xml`. Trước tiên, thêm vào `gleam.toml` của bạn:

```toml
[dependencies]
gleam_xml = "~> 1.0"
```

Bây giờ, phân tích cú pháp và tạo XML:

```rust
import gleam/xml

// Phân tích cú pháp XML
let doc = xml.parse("<note><to>Tove</to><from>Jani</from></note>")?

// Tạo XML
let node = xml.Element(
  "note",
  [],
  [
    xml.Element("to", [], [xml.Text("Tove")]),
    xml.Element("from", [], [xml.Text("Jani")]),
  ]
)
let xml_string = xml.render(node)
```

Kết quả mẫu cho `xml.render(node)` là:

```xml
<note><to>Tove</to><from>Jani</from></note>
```

## Sâu Hơn
XML là viết tắt của eXtensible Markup Language, một tiêu chuẩn từ W3C như là thành viên của HTML. Nó đã xuất hiện từ cuối những năm '90. Đối với Gleam, việc xử lý XML giống như bước trở lại quá khứ. JSON và Protocol Buffers thì thời thượng hơn, nhưng việc sử dụng rộng rãi của XML trong các hệ thống cũ kỹ và một số ngành cụ thể có nghĩa là nó vẫn còn liên quan.

Có các lựa chọn khác như `xmerl` tồn tại trong hệ sinh thái Erlang; tuy nhiên, thư viện `gleam_xml` cung cấp một cách tiếp cận phổ quát hơn cho người dùng Gleam. Nó được xây dựng dựa trên các thư viện Erlang hiện có nhưng cung cấp một API thân thiện với Gleam. Cách tiếp cận XML của Gleam nhằm mục đích đơn giản và an toàn, giảm bớt mã mẫu và nhấn mạnh về an toàn kiểu.

Về mặt thực hiện, các thư viện XML bao gồm `gleam_xml` thường cung cấp cấu trúc tương tự DOM. Điều này bao gồm các nút, thuộc tính và các phần tử lồng nhau, tận dụng mô hình khớp mẫu và bất đồng bộ của Erlang để xử lý các tài liệu có thể lớn và phức tạp.

## Xem Thêm
- Thư viện `gleam_xml` trên Hex: [https://hex.pm/packages/gleam_xml](https://hex.pm/packages/gleam_xml)
- Tiêu chuẩn XML chính thức của W3C: [https://www.w3.org/XML/](https://www.w3.org/XML/)
- Hướng dẫn XML toàn diện: [https://www.w3schools.com/xml/](https://www.w3schools.com/xml/)
- Tài liệu XML processing của Erlang `xmerl`: [http://erlang.org/doc/apps/xmerl/](http://erlang.org/doc/apps/xmerl/)
