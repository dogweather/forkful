---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:12:53.129827-07:00
description: "Vi\u1EBFt ki\u1EC3m th\u1EED ngh\u0129a l\xE0 t\u1EA1o ra m\xE3 ngu\u1ED3\
  n ki\u1EC3m tra xem m\xE3 ch\xEDnh c\u1EE7a b\u1EA1n ho\u1EA1t \u0111\u1ED9ng nh\u01B0\
  \ mong \u0111\u1EE3i hay kh\xF4ng. L\u1EADp tr\xECnh vi\xEAn ki\u1EC3m th\u1EED\
  \ \u0111\u1EC3 b\u1EAFt l\u1ED7i s\u1EDBm, \u0111\u1EA3m b\u1EA3o\u2026"
lastmod: '2024-03-13T22:44:36.548985-06:00'
model: gpt-4-0125-preview
summary: "Vi\u1EBFt ki\u1EC3m th\u1EED ngh\u0129a l\xE0 t\u1EA1o ra m\xE3 ngu\u1ED3\
  n ki\u1EC3m tra xem m\xE3 ch\xEDnh c\u1EE7a b\u1EA1n ho\u1EA1t \u0111\u1ED9ng nh\u01B0\
  \ mong \u0111\u1EE3i hay kh\xF4ng."
title: "Vi\u1EBFt ki\u1EC3m th\u1EED"
weight: 36
---

## Làm thế nào:
Elm sử dụng `elm-test` để viết kiểm thử. Dưới đây là một kiểm thử nhỏ cho hàm `add` tổng hợp hai số:

```Elm
import Expect
import Test exposing (..)
import AddingModule exposing (add)

suite : Test
suite =
    describe "AddingModule"
        [ test "kiểm thử hàm add" <|
            \_ -> Expect.equal (add 1 2) 3
        ]

-- Để chạy kiểm thử, sử dụng lệnh sau:
-- elm-test
```

Nếu hàm `add` hoạt động đúng, kết quả sẽ là:

```
TEST RUN PASSED

Thời lượng: 42 ms
Đã Pass:   1
Thất bại:   0
```

## Tìm hiểu sâu
Khung kiểm thử của Elm, `elm-test`, cung cấp một cách nhanh chóng, đáng tin cậy để viết kiểm thử đơn vị. Nó khuyến khích TDD (Phát triển Hướng Kiểm thử). Trước `elm-test`, đã có các phương án thay thế như `elm-check` nhưng không tích hợp mạnh mẽ. Về mặt triển khai, `elm-test` sử dụng hàm thuần khiết không có tác động phụ, phù hợp hoàn hảo với kiến trúc Elm.

## Xem thêm
- Tài liệu kiểm thử chính thức của Elm: https://package.elm-lang.org/packages/elm-explorations/test/latest/
- Một bài báo về mẫu kiểm thử của Elm: https://elmprogramming.com/testing.html
- Gói `elm-test` trên GitHub: https://github.com/elm-explorations/test
