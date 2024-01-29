---
title:                "Đọc một tệp văn bản"
date:                  2024-01-28T22:05:13.417609-07:00
model:                 gpt-4-0125-preview
simple_title:         "Đọc một tệp văn bản"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/gleam/reading-a-text-file.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Gì & Tại sao?
Đọc một tệp văn bản có nghĩa là lấy dữ liệu từ một tệp được tạo thành từ văn bản trên đĩa của bạn. Các lập trình viên làm điều này để truy cập và thao tác với thông tin được lưu trữ, như cấu hình, nhật ký, hoặc bất kỳ dữ liệu nào mà ứng dụng của họ cần.

## Làm thế nào:
Gleam không bao gồm IO tệp trong thư viện chuẩn của mình, vì vậy chúng ta sẽ sử dụng các hàm của Erlang. Chúng ta bắt đầu bằng cách mở một tệp văn bản sử dụng `file.open/2`, đọc nội dung của nó, xử lý các lỗi tiềm năng, và cuối cùng là đóng tệp. Dưới đây là cái chính của nó:

```gleam
import gleam/erlang
import gleam/result

fn main() {
  case erlang.file.open("example.txt", [read]) {
    Ok(file) ->
      case erlang.file.read(file) {
        Ok(data) -> {
          erlang.io.format("Nội dung: ~p~n", [data])
          erlang.file.close(file)
        }
        Error(err) -> {
          erlang.io.format("Lỗi khi đọc tệp: ~p~n", [err])
        }
      }
    Error(err) ->
      erlang.io.format("Lỗi khi mở tệp: ~p~n", [err])
  }
}
```

Chạy cái này và bạn sẽ thấy nội dung tệp văn bản của mình, hoặc một lỗi nếu có điều gì đó không đúng.

## Sâu hơn nữa
Đọc tệp không phải là điều mới; nó đã có trong lập trình từ những ngày của thẻ đục lỗ. Gleam, một ngôn ngữ đánh máy tĩnh biên dịch sang Erlang VM, dựa vào hệ sinh thái đã trưởng thành của Erlang cho các thao tác tệp. Bạn cũng có các lựa chọn khác, như: đọc bất đồng bộ, dòng phát trực tiếp, hoặc sử dụng các thư viện như `gleam_otp` cho một cách tiếp cận "Gleam-ish" hơn.

Hiểu về IO tệp bao gồm xử lý lỗi. Tệp có thể không tồn tại, có thể bị khóa, hoặc bạn có thể thiếu quyền. Sự phù hợp mẫu của Gleam và module `result` cung cấp cho bạn một con đường rõ ràng để quản lý những không mong đợi.

Cuối cùng, hãy xem xét kích cỡ của tệp của bạn. `erlang.file.read` đơn giản của chúng ta đọc toàn bộ nó vào bộ nhớ, có thể thành vấn đề đối với các tệp khổng lồ. Dòng phát trực tiếp từng phần hoặc dòng sẽ hiệu quả hơn.

## Xem thêm
- [Tài liệu module file của Erlang](http://erlang.org/doc/man/file.html) vì chúng ta đang sử dụng khả năng của Erlang.
- [Tài liệu IO của Erlang](http://erlang.org/doc/apps/stdlib/io_protocol.html) để hiểu cách thức hoạt động của nhập/xuất dưới nền.
