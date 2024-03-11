---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:11:41.110280-07:00
description: '#'
lastmod: '2024-03-11T00:14:10.675716-06:00'
model: gpt-4-0125-preview
summary: '#'
title: "L\xE0m vi\u1EC7c v\u1EDBi XML"
---

{{< edit_this_page >}}

## Làm việc với XML trong Ruby

### Làm gì & Tại sao?
Làm việc với XML nghĩa là phân tích cú pháp, tạo và thao tác với các tài liệu XML (eXtensible Markup Language) thông qua mã lệnh. Các lập trình viên làm điều này để tương tác với nhiều dịch vụ web, các tệp cấu hình, và các định dạng trao đổi dữ liệu nơi XML là ngôn ngữ chung.

### Làm thế nào:
Hãy sử dụng REXML, đi kèm với Ruby, để phân tích một đoạn XML:
```Ruby
require 'rexml/document'
include REXML

xml_data = <<-XML
<fruits>
  <fruit name="apple" color="green"/>
  <fruit name="banana" color="yellow"/>
</fruits>
XML

document = Document.new(xml_data)
document.elements.each('fruits/fruit') do |element|
  puts "Tên: #{element.attributes['name']}, Màu: #{element.attributes['color']}"
end
```
Kết quả xuất ra:
```
Tên: apple, Màu: green
Tên: banana, Màu: yellow
```

Tạo XML cũng khá thẳng thắn:
```Ruby
doc = Document.new
doc.add_element 'fruits'
apple = doc.root.add_element 'fruit', {'name' => 'apple', 'color' => 'green'}
banana = doc.root.add_element 'fruit', {'name' => 'banana', 'color' => 'yellow'}
puts doc
```
Kết quả XML:
```XML
<fruits>
  <fruit name="apple" color="green"/>
  <fruit name="banana" color="yellow"/>
</fruits>
```

### Tìm hiểu sâu:
Rễ của XML có từ những năm 1990 như một tập con đơn giản của SGML cho các tài liệu web. XML dài dòng nhưng cấu trúc cao, đó là lý do tại sao nó vẫn tồn tại. Không chỉ có mỗi XML - JSON và YAML trở nên phổ biến với sự đơn giản của chúng - nhưng XML vẫn mạnh mẽ trong nhiều hệ thống doanh nghiệp và hệ thống cũ.

Ruby cung cấp một vài cách để xử lý XML. REXML là một thư viện toàn Ruby dễ dàng bắt đầu. Nokogiri là một gem bao gồm các thư viện C nhanh hơn, cung cấp tốc độ và các tính năng bổ sung. Lựa chọn giữa chúng? Bắt đầu với REXML cho các nhiệm vụ nhỏ và chuyển sang Nokogiri nếu bạn cần nhiều công suất hơn.

Bên trong, phân tích cú pháp XML là về việc dịch chuỗi sang các mô hình DOM hoặc SAX. DOM tạo một cây trong bộ nhớ, trong khi SAX stream tài liệu và kích hoạt sự kiện khi nó phân tích. REXML cung cấp cả hai mô hình, nhưng thường chậm hơn so với các phần mở rộng C như những cái được sử dụng bởi Nokogiri.

### Xem thêm:
- Tài liệu Ruby REXML: https://www.rubydoc.info/stdlib/rexml
- Gem Nokogiri: https://nokogiri.org/
- Đặc tả XML: https://www.w3.org/XML/
- Giới thiệu về SAX: https://www.saxproject.org/
- So sánh YAML vs. JSON vs. XML: https://www.upwork.com/resources/json-vs-xml-vs-yaml
