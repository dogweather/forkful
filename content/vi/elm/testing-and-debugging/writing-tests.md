---
title:                "Viết kiểm thử"
aliases:
- /vi/elm/writing-tests.md
date:                  2024-01-28T22:12:53.129827-07:00
model:                 gpt-4-0125-preview
simple_title:         "Viết kiểm thử"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/elm/writing-tests.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Gì và Tại sao?
Viết kiểm thử nghĩa là tạo ra mã nguồn kiểm tra xem mã chính của bạn hoạt động như mong đợi hay không. Lập trình viên kiểm thử để bắt lỗi sớm, đảm bảo chức năng, và làm cho những thay đổi trong tương lai ít rủi ro hơn.

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
