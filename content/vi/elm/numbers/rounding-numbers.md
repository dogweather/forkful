---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:07:18.069462-07:00
description: "C\xE1ch l\xE0m: M\xF4-\u0111un `Basics` c\u1EE7a Elm cung c\u1EA5p nh\u1EEF\
  ng h\xE0m ti\u1EC7n \xEDch cho vi\u1EC7c l\xE0m tr\xF2n: `round`, `floor`, v\xE0\
  \ `ceiling`. D\u01B0\u1EDBi \u0111\xE2y l\xE0 c\xE1ch s\u1EED d\u1EE5ng ch\xFAng."
lastmod: '2024-03-13T22:44:36.537234-06:00'
model: gpt-4-0125-preview
summary: "M\xF4-\u0111un `Basics` c\u1EE7a Elm cung c\u1EA5p nh\u1EEFng h\xE0m ti\u1EC7\
  n \xEDch cho vi\u1EC7c l\xE0m tr\xF2n."
title: "L\xE0m tr\xF2n s\u1ED1"
weight: 13
---

## Cách làm:
Mô-đun `Basics` của Elm cung cấp những hàm tiện ích cho việc làm tròn: `round`, `floor`, và `ceiling`. Dưới đây là cách sử dụng chúng.

```elm
import Basics exposing (round, floor, ceiling)

-- Làm tròn về số nguyên gần nhất
round 3.14    --> 3
round 3.5     --> 4

-- Làm tròn xuống
floor 3.999   --> 3

-- Làm tròn lên
ceiling 3.001 --> 4

-- Cắt giảm phần thập phân mà không làm tròn
truncate 3.76 --> 3
```

Elm cũng cung cấp `toLocaleString` để làm tròn đến một số lượng chữ số thập phân cố định:

```elm
import Float exposing (toLocaleString)

-- Làm tròn đến hai chữ số thập phân
toLocaleString 2 3.14159 --> "3.14"
```

## Đi sâu hơn
Elm là một ngôn ngữ lập trình chức năng có kiểu mạnh mẽ, phân chia hiệu ứng phụ ra ngoài cạnh kiến trúc. Điều này đồng nghĩa với việc các hàm như làm tròn phải là pure và dự đoán được. Trong lịch sử, việc làm tròn là một thao tác phổ biến trong nhiều ngôn ngữ lập trình khi đối mặt với sự không chính xác của số học dấu phẩy động.

Cách tiếp cận của Elm đối với việc làm tròn rất đơn giản - các hàm là pure và tuân thủ theo định nghĩa toán học về round, floor, và ceiling. Elm dự đoán nhu cầu phổ biến bằng cách cung cấp các hàm sẵn có, vì việc quản lý độ chính xác là yêu cầu thường xuyên, đặc biệt là trong tài chính và đồ họa.

Các phương án khác cho các hàm sẵn có của Elm có thể bao gồm việc triển khai tùy chỉnh sử dụng các phép toán số học, nhưng điều đó sẽ thêm vào sự phức tạp không cần thiết khi thư viện chuẩn đã thực hiện công việc một cách hiệu quả.

Tính đến phiên bản hiện tại, Elm sử dụng toán số học dấu phẩy động cơ bản của JavaScript cho các thao tác này, do đó, giữ cho sự nhất quán với tiêu chuẩn IEEE 754, điều này là cái cần ghi nhớ khi xem xét về độ chính xác và khả năng lỗi số học dấu phẩy động.

## Xem thêm
- Tài liệu chính thức của mô-đun `Basics` của Elm: https://package.elm-lang.org/packages/elm/core/latest/Basics
- Một cái nhìn chi tiết vào cách hoạt động của số thập phân dấu phẩy động trong máy tính: https://floating-point-gui.de/
- Mô-đun `Float` của Elm cho thêm các thao tác số học dấu phẩy động: https://package.elm-lang.org/packages/elm/core/latest/Float
