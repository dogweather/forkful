---
aliases:
- /vi/elm/printing-debug-output/
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:04:40.537775-07:00
description: "In th\xF4ng tin g\u1EE1 l\u1ED7i trong Elm l\xE0 vi\u1EC7c hi\u1EC3\
  n th\u1ECB c\xE1c gi\xE1 tr\u1ECB tr\xEAn b\u1EA3ng \u0111i\u1EC1u khi\u1EC3n \u0111\
  \u1EC3 hi\u1EC3u \u0111i\u1EC1u g\xEC \u0111ang x\u1EA3y ra trong m\xE3 c\u1EE7\
  a b\u1EA1n. Ch\xFAng t\xF4i l\xE0m \u0111i\u1EC1u n\xE0y \u0111\u1EC3\u2026"
lastmod: 2024-02-18 23:08:50.606539
model: gpt-4-0125-preview
summary: "In th\xF4ng tin g\u1EE1 l\u1ED7i trong Elm l\xE0 vi\u1EC7c hi\u1EC3n th\u1ECB\
  \ c\xE1c gi\xE1 tr\u1ECB tr\xEAn b\u1EA3ng \u0111i\u1EC1u khi\u1EC3n \u0111\u1EC3\
  \ hi\u1EC3u \u0111i\u1EC1u g\xEC \u0111ang x\u1EA3y ra trong m\xE3 c\u1EE7a b\u1EA1\
  n. Ch\xFAng t\xF4i l\xE0m \u0111i\u1EC1u n\xE0y \u0111\u1EC3\u2026"
title: "In ra th\xF4ng tin g\u1EE1 l\u1ED7i"
---

{{< edit_this_page >}}

## Gì và Tại sao?

In thông tin gỡ lỗi trong Elm là việc hiển thị các giá trị trên bảng điều khiển để hiểu điều gì đang xảy ra trong mã của bạn. Chúng tôi làm điều này để bắt lỗi và đảm bảo luồng lôgic hoạt động như ý muốn.

## Cách thực hiện:

Elm không có hàm `print` sẵn có như một số ngôn ngữ khác, nhưng bạn có thể sử dụng mô-đun `Debug` cho đầu ra bảng điều khiển:

```Elm
import Debug

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  model
    |> Debug.log "model trước khi cập nhật"
    |> actualUpdateFunction msg
    |> Debug.log "model sau khi cập nhật"
```

Bạn sẽ thấy điều gì đó như thế này trên bảng điều khiển trình duyệt của mình:

```
model trước khi cập nhật: { ... một số dữ liệu mô hình ... }
model sau khi cập nhật: { ... một số dữ liệu mô hình đã cập nhật ... }
```

Nhớ rằng, hàm `Debug.log` rất hữu ích, nhưng đừng vận chuyển mã code của bạn với nó. Elm sẽ nhắc bạn loại bỏ các câu lệnh gỡ lỗi trước khi bạn có thể tạo bản build sản xuất.

## Sâu hơn

`Debug.log` là một phần của mô-đun `Debug` của Elm, được thiết kế chỉ dành cho trợ giúp thời gian phát triển. Lịch sử, Elm đã nhấn mạnh vào việc tập trung vào bảo trì và xử lý lỗi, giữ cho mô-đun `Debug` cố ý đơn giản. Sự đơn giản đảm bảo rằng các nhà phát triển tập trung vào đầu ra có ý nghĩa thay vì lạc vào một bộ gỡ lỗi phức tạp.

Hàm `Debug.log` của Elm nhận hai đối số: một thẻ chuỗi và dữ liệu để log ra. Đầu ra sau đó được in ra bảng điều khiển của trình duyệt. Các phương án thay thế cho cách tiếp cận này bao gồm:

1. Ghi nhật ký bảng điều khiển truyền thống: Elm không hỗ trợ ghi nhật ký bảng điều khiển trực tiếp do kiến trúc của Elm hướng đến việc không có ngoại lệ thời gian chạy, và việc ghi nhật ký trực tiếp có thể phá vỡ đảm bảo này.
2. Bộ Gỡ lỗi Dừng Thời gian của Elm: Công cụ này cho phép bạn hình dung trạng thái của ứng dụng của mình theo thời gian mà không cần nhật ký bảng điều khiển và là một cách mạnh mẽ để gỡ lỗi các ứng dụng phức tạp.

Về mặt triển khai, hàm `Debug.log` bao bọc dữ liệu của bạn với một thẻ định danh. Điều này có ích để phân biệt các điểm dữ liệu khác nhau. Trong sản xuất, trình biên dịch Elm sẽ đánh dấu bất kỳ việc sử dụng nào của `Debug.log`, đảm bảo bạn giữ mã sản xuất của mình sạch sẽ khỏi các tác phẩm gỡ lỗi.

## Xem Thêm

- Hướng dẫn chính thức về gỡ lỗi của Elm: https://guide.elm-lang.org/debugging/
- Giới thiệu về Bộ Gỡ lỗi Dừng Thời gian: https://elm-lang.org/news/the-perfect-bug-report
- Tài liệu mô-đun Debug của Elm: https://package.elm-lang.org/packages/elm/core/latest/Debug
