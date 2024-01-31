---
title:                "Làm việc với XML"
date:                  2024-01-28T22:11:16.873612-07:00
model:                 gpt-4-0125-preview
simple_title:         "Làm việc với XML"

category:             "C"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/c/working-with-xml.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Làm Thế Nào & Tại Sao?
Làm việc với XML trong C bao gồm việc phân tích cú pháp, tạo và thao tác với các tệp XML - cơ bản là lưu trữ dữ liệu có cấu trúc. Các lập trình viên làm điều này để tương tác với dữ liệu trong một định dạng dễ di chuyển và dễ đọc cho con người, thường được sử dụng cho cấu hình, trao đổi dữ liệu, và nhiều hơn nữa.

## Cách Thực Hiện:
Dưới đây là một đoạn mã sử dụng thư viện `libxml2` để phân tích cú pháp một tệp XML và lấy phần tử gốc.

```C
#include <stdio.h>
#include <libxml/parser.h>
#include <libxml/tree.h>

int main() {
    xmlDoc *doc = NULL;
    xmlNode *root_element = NULL;

    // Phân tích cú pháp tệp XML
    doc = xmlReadFile("example.xml", NULL, 0);

    // Lấy phần tử gốc
    root_element = xmlDocGetRootElement(doc);

    printf("Phần Tử Gốc: %s\n", root_element->name);

    // Giải phóng tài liệu
    xmlFreeDoc(doc);

    // Dọn dẹp parser
    xmlCleanupParser();

    return 0;
}
```

Kết quả mẫu cho một XML với gốc `<data>` có thể sẽ là:
```
Phần Tử Gốc: data
```

## Đào Sâu
XML, hay Ngôn ngữ Đánh dấu Mở rộng, có từ cuối những năm '90, cung cấp một cách để mô tả và cấu trúc dữ liệu. Trong C, `libxml2` là lựa chọn hàng đầu. Nó mạnh mẽ, mặc dù không dễ dàng nhất cho người mới. Các lựa chọn khác bao gồm `tinyxml2`, nhẹ hơn và thân thiện với người mới bắt đầu hơn. Về thực hiện, C không có hỗ trợ XML sẵn có, vì vậy các thư viện lấp đầy khoảng trống. Chúng thay đổi về kích thước, tốc độ, độ phức tạp, và khả năng di động. Hầu hết cung cấp các phương thức phân tích cú pháp DOM và SAX: DOM tải toàn bộ vào bộ nhớ, tốt cho các tài liệu nhỏ; SAX được điều khiển bởi sự kiện, xử lý các phần tử ngay lập tức, tốt hơn cho các tệp lớn. Cả hai đều có các trường hợp sử dụng và sự đánh đổi của chúng.

## Xem Thêm
- [libxml2](http://xmlsoft.org/)
- [tinyxml2 trên GitHub](https://github.com/leethomason/tinyxml2)
- [Hướng dẫn XML trên w3schools](https://www.w3schools.com/xml/)
- [Thông số kỹ thuật XML của W3C](https://www.w3.org/XML/)
