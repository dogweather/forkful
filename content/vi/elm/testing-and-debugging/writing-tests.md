---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:12:53.129827-07:00
description: "L\xE0m th\u1EBF n\xE0o: Elm s\u1EED d\u1EE5ng `elm-test` \u0111\u1EC3\
  \ vi\u1EBFt ki\u1EC3m th\u1EED. D\u01B0\u1EDBi \u0111\xE2y l\xE0 m\u1ED9t ki\u1EC3\
  m th\u1EED nh\u1ECF cho h\xE0m `add` t\u1ED5ng h\u1EE3p hai s\u1ED1."
lastmod: '2024-03-13T22:44:36.548985-06:00'
model: gpt-4-0125-preview
summary: "Elm s\u1EED d\u1EE5ng `elm-test` \u0111\u1EC3 vi\u1EBFt ki\u1EC3m th\u1EED\
  ."
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
