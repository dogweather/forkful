---
title:                "Sử dụng mảng liên kết"
aliases:
- /vi/powershell/using-associative-arrays/
date:                  2024-01-30T19:13:02.777335-07:00
model:                 gpt-4-0125-preview
simple_title:         "Sử dụng mảng liên kết"

tag:                  "Data Structures"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/powershell/using-associative-arrays.md"
changelog:
  - 2024-01-30, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cái gì & Tại sao?

Mảng kết hợp, còn được biết đến như bảng băm hoặc từ điển trong PowerShell, cho phép bạn lưu trữ dữ liệu dưới dạng cặp khóa-giá trị, giúp việc truy xuất dữ liệu trở nên đơn giản và hiệu quả. Các lập trình viên sử dụng chúng để lưu trữ dữ liệu liên quan lại với nhau một cách dễ dàng để truy cập qua khóa.

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
