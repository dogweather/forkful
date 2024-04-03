---
changelog:
- 2024-01-30, gpt-4-0125-preview, translated from English
date: 2024-01-30 19:13:02.777335-07:00
description: "L\xE0m th\u1EBF n\xE0o: Vi\u1EC7c t\u1EA1o v\xE0 s\u1EED d\u1EE5ng m\u1EA3\
  ng k\u1EBFt h\u1EE3p trong PowerShell kh\xE1 l\xE0 \u0111\u01A1n gi\u1EA3n. D\u01B0\
  \u1EDBi \u0111\xE2y l\xE0 c\xE1ch b\u1EA1n th\u1EF1c hi\u1EC7n ph\xE9p thu\u1EAD\
  t: **T\u1EA1o m\u1ED9t m\u1EA3ng k\u1EBFt h\u1EE3p:**."
lastmod: '2024-03-13T22:44:36.924006-06:00'
model: gpt-4-0125-preview
summary: "Vi\u1EC7c t\u1EA1o v\xE0 s\u1EED d\u1EE5ng m\u1EA3ng k\u1EBFt h\u1EE3p trong\
  \ PowerShell kh\xE1 l\xE0 \u0111\u01A1n gi\u1EA3n."
title: "S\u1EED d\u1EE5ng m\u1EA3ng li\xEAn k\u1EBFt"
weight: 15
---

## Làm thế nào:
Việc tạo và sử dụng mảng kết hợp trong PowerShell khá là đơn giản. Dưới đây là cách bạn thực hiện phép thuật:

**Tạo một mảng kết hợp:**

```PowerShell
$myAssociativeArray = @{}
$myAssociativeArray["name"] = "Alex"
$myAssociativeArray["age"] = 25
$myAssociativeArray["job"] = "Kỹ sư"
```

Đoạn mã này tạo một mảng kết hợp với ba cặp khóa-giá trị.

**Truy cập giá trị:**

Để lấy một giá trị, tham chiếu đến khóa của nó:

```PowerShell
Write-Output $myAssociativeArray["name"]
```

**Kết quả mẫu:**

```
Alex
```

**Thêm hoặc chỉnh sửa dữ liệu:**

Chỉ cần sử dụng khóa để thêm một cặp mới hoặc chỉnh sửa một cặp hiện có:

```PowerShell
$myAssociativeArray["location"] = "New York" # Thêm một cặp khóa-giá trị mới
$myAssociativeArray["job"] = "Kỹ sư Cao cấp" # Chỉnh sửa một cặp đã có
```

**Lặp qua một mảng kết hợp:**

Lặp qua các khóa và giá trị như thế này:

```PowerShell
foreach ($key in $myAssociativeArray.Keys) {
  $value = $myAssociativeArray[$key]
  Write-Output "$key : $value"
}
```

**Kết quả mẫu:**

```
name : Alex
age : 25
job : Kỹ sư Cao cấp
location : New York
```

## Sâu hơn nữa
Khái niệm về mảng kết hợp phổ biến trong nhiều ngôn ngữ lập trình, thường được gọi là từ điển, bản đồ hoặc bảng băm tuỳ thuộc vào ngôn ngữ. Trong PowerShell, mảng kết hợp được thực hiện dưới dạng bảng băm, rất hiệu quả để tra cứu khóa, lưu trữ dữ liệu và duy trì một bộ sưu tập khóa duy nhất.

Trong lịch sử, mảng kết hợp cung cấp một phương tiện để quản lý các bộ sưu tập đối tượng, mỗi đối tượng có thể được truy xuất nhanh chóng mà không cần lặp qua toàn bộ bộ sưu tập, sử dụng khóa của nó. Hiệu quả của việc truy xuất và chỉnh sửa dữ liệu trong mảng kết hợp làm chúng trở thành lựa chọn ưu tiên cho các nhiệm vụ khác nhau. Tuy nhiên, chúng cũng có hạn chế, như việc duy trì trật tự, cho nên từ điển được sắp xếp hoặc đối tượng tuỳ chỉnh có thể là một lựa chọn tốt hơn.

Bất chấp các hạn chế, mảng kết hợp/bảng băm trong PowerShell cực kỳ linh hoạt và là công cụ mạnh mẽ cho việc scripting. Chúng cho phép lưu trữ dữ liệu động và đặc biệt hữu ích trong cấu hình, thao tác dữ liệu, và ở bất cứ đâu một định dạng dữ liệu có cấu trúc được cần mà không gặp phải gánh nặng của một định nghĩa lớp chính thức. Chỉ cần nhớ rằng, mặc dù mảng kết hợp thích hợp cho việc truy xuất dựa trên khóa, nếu nhiệm vụ của bạn bao gồm cấu trúc dữ liệu phức tạp hoặc yêu cầu duy trì một trật tự cụ thể, bạn có thể muốn khám phá các kiểu dữ liệu khác hoặc đối tượng tuỳ chỉnh trong PowerShell.
