---
title:                "Tạo một tập tin tạm thời"
aliases: - /vi/elm/creating-a-temporary-file.md
date:                  2024-01-28T21:58:25.769178-07:00
model:                 gpt-4-0125-preview
simple_title:         "Tạo một tập tin tạm thời"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/elm/creating-a-temporary-file.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

# Tạo file tạm thời với Elm

## Cái gì & Tại sao?

Tạo một file tạm thời có nghĩa là tạo một file được thiết kế cho việc sử dụng ngắn hạn. Các lập trình viên làm điều này vì một số lý do như bảo vệ dữ liệu nhạy cảm hoặc quản lý kết quả tạm thời trong quá trình xử lý.

## Cách thực hiện:

Elm chạy trong trình duyệt, vì vậy nó không có quyền truy cập hệ thống file trực tiếp. Do đó, bạn không thể tạo file tạm thời theo cách truyền thống. Nhưng, nếu bạn cần một tính năng tương tự, chúng tôi sử dụng cổng Elm (Elm ports) để tương tác với JavaScript, có thể xử lý việc tạo file tạm thời.

```elm
port module Main exposing (..)

-- Định nghĩa một cổng để tạo file tạm thời trong JavaScript
port createTempFile : String -> Cmd msg

-- Gửi dữ liệu sang JavaScript để tạo file tạm thời
saveDataTemporarily : String -> Cmd msg
saveDataTemporarily data =
    createTempFile data
```

Đối với phần JavaScript, sử dụng File API:

```javascript
app.ports.createTempFile.subscribe(function(data) {
    var blob = new Blob([data], {type: 'text/plain'});
    var url = URL.createObjectURL(blob);

    // Tại đây bạn có thể sử dụng URL để tải xuống blob hoặc chuyển nó sang các phần khác của ứng dụng
    console.log(url);  // Nó ghi log URL của file tạm thời
});
```

Mẫu đầu ra trong bảng điều khiển JavaScript:

```plaintext
blob:null/2135a9b7-1aad-4e7a-8bce-19c4f3f6d7ff
```

## Tìm hiểu sâu hơn

Elm được thiết kế để an toàn và đáng tin cậy, vì vậy quyền truy cập hệ thống file trực tiếp không có trong danh sách. Thay vào đó, Elm sử dụng cổng để giao tiếp với JavaScript, cho phép các thao tác như tạo file tạm thời. Theo lịch sử, chúng tôi xử lý các nhiệm vụ dựa trên file trong trình duyệt thông qua các API của JavaScript, sử dụng Elm cho logic cấp cao, an toàn về kiểu.

Các phương án khác như WebAssembly có thể cho phép tương tác hệ thống file trực tiếp nhiều hơn trong tương lai, nhưng hiện nay, tương tác với JavaScript là thực hành tiêu chuẩn.

Về mặt triển khai, việc tạo file tạm thời trong bối cảnh trình duyệt không có nghĩa là một file thực sự trên hệ thống file, mà là một biểu diễn trong bộ nhớ (blob) mà bạn có thể làm việc và lưu trữ nếu cần.

## Xem thêm

- [Cổng Elm](https://guide.elm-lang.org/interop/ports.html)
- [MDN - Web APIs - File](https://developer.mozilla.org/en-US/docs/Web/API/File)
- [MDN - Web APIs - Blob](https://developer.mozilla.org/en-US/docs/Web/API/Blob)
