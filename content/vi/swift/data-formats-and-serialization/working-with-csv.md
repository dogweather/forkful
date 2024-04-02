---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:10:21.491190-07:00
description: "T\u1EC7p CSV (Comma-Separated Values - Gi\xE1 tr\u1ECB \u0111\u01B0\u1EE3\
  c ph\xE2n t\xE1ch b\u1EB1ng d\u1EA5u ph\u1EA9y) l\xE0 c\xE1c t\u1EC7p v\u0103n b\u1EA3\
  n \u0111\u01A1n gi\u1EA3n \u0111\u1EC3 l\u01B0u tr\u1EEF d\u1EEF li\u1EC7u d\u1EA1\
  ng b\u1EA3ng. L\u1EADp tr\xECnh vi\xEAn s\u1EED d\u1EE5ng\u2026"
lastmod: '2024-03-13T22:44:37.127191-06:00'
model: gpt-4-0125-preview
summary: "T\u1EC7p CSV (Comma-Separated Values - Gi\xE1 tr\u1ECB \u0111\u01B0\u1EE3\
  c ph\xE2n t\xE1ch b\u1EB1ng d\u1EA5u ph\u1EA9y) l\xE0 c\xE1c t\u1EC7p v\u0103n b\u1EA3\
  n \u0111\u01A1n gi\u1EA3n \u0111\u1EC3 l\u01B0u tr\u1EEF d\u1EEF li\u1EC7u d\u1EA1\
  ng b\u1EA3ng. L\u1EADp tr\xECnh vi\xEAn s\u1EED d\u1EE5ng\u2026"
title: "L\xE0m vi\u1EC7c v\u1EDBi CSV"
weight: 37
---

## Cái gì & Tại sao?

Tệp CSV (Comma-Separated Values - Giá trị được phân tách bằng dấu phẩy) là các tệp văn bản đơn giản để lưu trữ dữ liệu dạng bảng. Lập trình viên sử dụng chúng bởi vì chúng dễ đọc và viết, và được hỗ trợ rộng rãi trên các hệ thống và ngôn ngữ.

## Làm thế nào:

Hãy đọc và phân tích nội dung một tệp CSV trong Swift.

Đầu tiên, giả sử chúng ta có một tệp `data.csv` với nội dung sau:

```plaintext
name,age,city
Alice,30,New York
Bob,25,Los Angeles
```

Dưới đây là một đoạn mã Swift cơ bản để đọc và phân tích nó:

```swift
import Foundation

let csvContent = """
name,age,city
Alice,30,New York
Bob,25,Los Angeles
"""

var rows = csvContent.components(separatedBy: "\n")
let headers = rows.removeFirst().components(separatedBy: ",")

var data = [[String: String]]()

for row in rows {
    let columns = row.components(separatedBy: ",")
    var rowData = [String: String]()
    for (header, column) in zip(headers, columns) {
        rowData[header] = column
    }
    data.append(rowData)
}

print(data)
```

Đầu ra mẫu:

```plaintext
[["name": "Alice", "age": "30", "city": "New York"], ["name": "Bob", "age": "25", "city": "Los Angeles"]]
```

## Sâu hơn

CSV đã tồn tại từ những ngày đầu của máy tính - được sử dụng để chuyển dữ liệu giữa các chương trình, cơ sở dữ liệu và hệ thống. Các lựa chọn khác như JSON và XML tồn tại, nhưng CSV vẫn phổ biến vì sự đơn giản của nó. Về hiệu quả, phương thức `String` của Swift xử lý tốt CSV đối với các tập dữ liệu nhỏ, nhưng dữ liệu quy mô lớn có thể cần một thư viện chuyên biệt như SwiftCSV hoặc CodableCSV để có hiệu suất và sự tiện lợi.

## Xem thêm

- Tài liệu Swift của Apple cho việc thao tác với String: [https://developer.apple.com/documentation/swift/string](https://developer.apple.com/documentation/swift/string)
- SwiftCSV, một thư viện CSV dành riêng cho Swift: [https://github.com/swiftcsv/SwiftCSV](https://github.com/swiftcsv/SwiftCSV)
- CodableCSV, một bộ mã hóa/giải mã CSV cho Swift: [https://github.com/dehesa/CodableCSV](https://github.com/dehesa/CodableCSV)
