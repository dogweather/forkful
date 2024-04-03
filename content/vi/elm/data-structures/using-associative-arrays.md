---
changelog:
- 2024-01-30, gpt-4-0125-preview, translated from English
date: 2024-01-30 19:11:28.698131-07:00
description: "L\xE0m th\u1EBF n\xE0o: Trong Elm, b\u1EA1n l\xE0m vi\u1EC7c v\u1EDB\
  i T\u1EEB \u0111i\u1EC3n trong module `Dict`, v\u1EADy ch\xFAng ta h\xE3y c\xF9\
  ng nhau xem m\u1ED9t v\xED d\u1EE5 nhanh."
lastmod: '2024-03-13T22:44:36.534518-06:00'
model: gpt-4-0125-preview
summary: "Trong Elm, b\u1EA1n l\xE0m vi\u1EC7c v\u1EDBi T\u1EEB \u0111i\u1EC3n trong\
  \ module `Dict`, v\u1EADy ch\xFAng ta h\xE3y c\xF9ng nhau xem m\u1ED9t v\xED d\u1EE5\
  \ nhanh."
title: "S\u1EED d\u1EE5ng m\u1EA3ng li\xEAn k\u1EBFt"
weight: 15
---

## Làm thế nào:
Trong Elm, bạn làm việc với Từ điển trong module `Dict`, vậy chúng ta hãy cùng nhau xem một ví dụ nhanh:

```Elm
import Dict exposing (Dict)

-- Khởi tạo một từ điển với các khóa là Chuỗi và các giá trị là Số nguyên
exampleDict : Dict String Int
exampleDict = Dict.fromList [("apple", 5), ("banana", 2), ("orange", 8)]

-- Thêm hoặc cập nhật một giá trị
updatedDict = Dict.insert "grape" 10 exampleDict

-- Truy xuất một giá trị (chú ý kiểu Maybe, vì khóa có thể không tồn tại)
fruitCount : Maybe Int
fruitCount = Dict.get "apple" updatedDict

-- Xóa một cặp khóa-giá trị
finalDict = Dict.remove "banana" updatedDict

-- Chuyển đổi một từ điển trở lại thành danh sách
dictToList = Dict.toList finalDict
```

Kết quả mẫu khi hiển thị `dictToList`:

```Elm
[("apple", 5), ("grape", 10), ("orange", 8)]
```

Điều này minh họa các thao tác cơ bản: tạo, cập nhật, truy cập, và duyệt qua một Từ điển.

## Sâu hơn nữa
Từ điển trong Elm nội bộ sử dụng một cấu trúc được biết đến là cây AVL - một loại cây tìm kiếm nhị phân tự cân bằng. Lựa chọn này tạo ra một sự cân bằng giữa việc đảm bảo rằng các hoạt động như chèn, get, và remove có hiệu suất tốt (độ phức tạp thời gian là logarithmic) và duy trì sự đơn giản trong việc xử lý dữ liệu.

Mặc dù có những điểm mạnh của `Dict` của Elm, nó không phải là một giải pháp phù hợp với mọi trường hợp. Đối với các tập hợp có thứ tự hoặc cần được duyệt qua một cách tuần tự, Danh sách hoặc Mảng có thể phù hợp hơn. Hơn nữa, khi làm việc với một tập hợp cố định của các khóa đã biết, sử dụng các kiểu tùy chỉnh (phiên bản của các kiểu liệt kê trong Elm) có thể cung cấp nhiều độ an toàn kiểu và rõ ràng hơn trong mã của bạn.

Trong hệ sinh thái của Elm, `Dict` cung cấp một cách đáng tin cậy để quản lý các tập hợp các cặp khóa-giá trị nơi các khóa là duy nhất và thứ tự không quan trọng. Dù có thể xuất hiện những cấu trúc mới hay phức tạp hơn, module `Dict` vẫn là một công cụ cơ bản trong bộ công cụ của lập trình viên Elm vì sự đơn giản và hiệu quả của nó trong việc xử lý mảng liên kết.
