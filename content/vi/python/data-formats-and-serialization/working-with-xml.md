---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:11:40.942793-07:00
description: "L\xE0m Th\u1EBF N\xE0o: M\xF4-\u0111un `xml.etree.ElementTree` c\u1EE7\
  a Python cung c\u1EA5p c\xF4ng c\u1EE5 \u0111\u1EC3 l\xE0m vi\u1EC7c v\u1EDBi XML.\
  \ Ph\xE2n t\xEDch t\xE0i li\u1EC7u XML."
lastmod: '2024-03-13T22:44:36.129133-06:00'
model: gpt-4-0125-preview
summary: "M\xF4-\u0111un `xml.etree.ElementTree` c\u1EE7a Python cung c\u1EA5p c\xF4\
  ng c\u1EE5 \u0111\u1EC3 l\xE0m vi\u1EC7c v\u1EDBi XML."
title: "L\xE0m vi\u1EC7c v\u1EDBi XML"
weight: 40
---

## Làm Thế Nào:
Mô-đun `xml.etree.ElementTree` của Python cung cấp công cụ để làm việc với XML.

Phân tích tài liệu XML:
```python
import xml.etree.ElementTree as ET

xml_data = """<?xml version="1.0"?>
<library>
    <book>
        <title>Learning Python</title>
        <author>Mark Lutz</author>
    </book>
    <book>
        <title>Programming Python</title>
        <author>Mark Lutz</author>
    </book>
</library>
"""

root = ET.fromstring(xml_data)
for book in root.findall('book'):
    title = book.find('title').text
    author = book.find('author').text
    print(f'Tiêu đề: {title}, Tác giả: {author}')
```
Kết quả mẫu:
```
Tiêu đề: Learning Python, Tác giả: Mark Lutz
Tiêu đề: Programming Python, Tác giả: Mark Lutz
```

Tạo tài liệu XML:
```python
library = ET.Element('library')
book = ET.SubElement(library, 'book')
title = ET.SubElement(book, 'title')
title.text = 'Automate the Boring Stuff with Python'
author = ET.SubElement(book, 'author')
author.text = 'Al Sweigart'

tree = ET.ElementTree(library)
tree.write('library.xml')
```

## Sâu Hơn:
XML đã xuất hiện từ cuối những năm '90, được tạo ra như một tập con đơn giản hóa của SGML dễ dàng chia sẻ dữ liệu trực tuyến. Dù sự phổ biến của JSON đang tăng cao đối với dữ liệu web, XML vẫn rất quan trọng trong nhiều doanh nghiệp, cấu hình và dịch vụ web (SOAP, RSS).

Những lựa chọn thay thế `xml.etree.ElementTree` bao gồm `lxml` và `minidom`. `lxml` nhanh hơn và có nhiều tính năng hơn, trong khi `minidom` cung cấp giao diện XML giống "DOM" hơn. Khi chọn, cân nhắc về sự dễ sử dụng, hiệu suất và yêu cầu về tính năng cụ thể.

Ở dưới cùng, `ElementTree` hoạt động trên mô hình cây phần tử, nơi mỗi thành phần của tệp XML là một nút trong cây. Điều này cho phép các biểu thức đường dẫn và tìm kiếm đơn giản, làm cho việc điều hướng và thao tác cấu trúc dữ liệu XML trở nên dễ dàng hơn.

## Xem Thêm:
- Mô-đun `xml.etree.ElementTree` của Python: https://docs.python.org/3/library/xml.etree.elementtree.html
- `lxml`: https://lxml.de/
- Hướng dẫn XML của W3Schools: https://www.w3schools.com/xml/
- Đặc tả XML: https://www.w3.org/XML/
