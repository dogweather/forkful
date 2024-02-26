---
changelog:
- 2024-01-30, gpt-4-0125-preview, translated from English
date: 2024-01-30 19:11:28.698131-07:00
description: "M\u1EA3ng li\xEAn k\u1EBFt, hay nh\u01B0 Elm g\u1ECDi l\xE0, T\u1EEB\
  \ \u0111i\u1EC3n, \xE1nh x\u1EA1 c\xE1c kh\xF3a v\u1EDBi c\xE1c gi\xE1 tr\u1ECB\
  \ theo c\xE1ch l\xE0m cho vi\u1EC7c tra c\u1EE9u, ch\xE8n, v\xE0 x\xF3a c\xE1c gi\xE1\
  \ tr\u1ECB tr\u1EDF n\xEAn c\u1EF1c k\u1EF3 nhanh\u2026"
lastmod: '2024-02-25T18:49:34.874518-07:00'
model: gpt-4-0125-preview
summary: "M\u1EA3ng li\xEAn k\u1EBFt, hay nh\u01B0 Elm g\u1ECDi l\xE0, T\u1EEB \u0111\
  i\u1EC3n, \xE1nh x\u1EA1 c\xE1c kh\xF3a v\u1EDBi c\xE1c gi\xE1 tr\u1ECB theo c\xE1\
  ch l\xE0m cho vi\u1EC7c tra c\u1EE9u, ch\xE8n, v\xE0 x\xF3a c\xE1c gi\xE1 tr\u1ECB\
  \ tr\u1EDF n\xEAn c\u1EF1c k\u1EF3 nhanh\u2026"
title: "S\u1EED d\u1EE5ng m\u1EA3ng li\xEAn k\u1EBFt"
---

{{< edit_this_page >}}

## Cái gì & Tại sao?

Mảng liên kết, hay như Elm gọi là, Từ điển, ánh xạ các khóa với các giá trị theo cách làm cho việc tra cứu, chèn, và xóa các giá trị trở nên cực kỳ nhanh chóng. Chúng là lựa chọn hàng đầu của bạn khi bạn cần theo dõi các đối tượng mà không cần theo một trật tự nghiêm ngặt, như là sở thích của người dùng hay danh sách hàng tồn kho.

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
