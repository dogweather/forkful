---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:10:21.491190-07:00
description: "L\xE0m th\u1EBF n\xE0o: H\xE3y \u0111\u1ECDc v\xE0 ph\xE2n t\xEDch n\u1ED9\
  i dung m\u1ED9t t\u1EC7p CSV trong Swift. \u0110\u1EA7u ti\xEAn, gi\u1EA3 s\u1EED\
  \ ch\xFAng ta c\xF3 m\u1ED9t t\u1EC7p `data.csv` v\u1EDBi n\u1ED9i dung sau."
lastmod: '2024-03-13T22:44:37.127191-06:00'
model: gpt-4-0125-preview
summary: "H\xE3y \u0111\u1ECDc v\xE0 ph\xE2n t\xEDch n\u1ED9i dung m\u1ED9t t\u1EC7\
  p CSV trong Swift."
title: "L\xE0m vi\u1EC7c v\u1EDBi CSV"
weight: 37
---

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
