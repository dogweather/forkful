---
title:                "Kiểm tra xem thư mục có tồn tại không"
date:                  2024-01-28T21:56:39.042511-07:00
model:                 gpt-4-0125-preview
simple_title:         "Kiểm tra xem thư mục có tồn tại không"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/gleam/checking-if-a-directory-exists.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Gì và Tại Sao?

Việc kiểm tra xem một thư mục có tồn tại hay không giống như liếc vào một căn phòng để xem nó có ở đó trước khi bạn bước vào. Lập trình viên làm vậy để tránh lỗi khi cố gắng truy cập hoặc chỉnh sửa các thư mục có thể không có mặt.

## Làm thế nào:

Để kiểm tra xem một thư mục có tồn tại trong Gleam hay không, chúng ta cần tương tác với hệ thống tệp tin. Thật không may, tính đến thời điểm kiến thức cuối cùng của tôi vào năm 2023, Gleam không có các thao tác hệ thống tệp tin được xây dựng sẵn vì nó chủ yếu được thiết kế để xây dựng các hệ thống chịu lỗi. Tuy nhiên, chúng ta có thể giao tiếp với các thao tác hệ thống tệp tin của Erlang nhờ vào khả năng sử dụng các hàm Erlang của Gleam.

Dưới đây là một ví dụ nhanh sử dụng module `file` của Erlang:

```Gleam
import gleam/erlang
import gleam/io

fn does_directory_exist(dir: String) -> Bool {
  case erlang.apply(
    module: "file", 
    function: "read_file_info", 
    args: [dir]
  ) {
    Ok(_) -> true
    Error(_) -> false
  }
}

fn main() {
  let directory = "/some/path/to/directory"
  let exists = does_directory_exist(directory)
  io.println(exists)
}
```

Điều này có thể xuất ra `true` nếu thư mục tồn tại, và `false` nếu nó không tồn tại.

## Sâu hơn

Về lịch sử, Gleam còn trẻ và đang phát triển. Nó được xây dựng trên máy ảo BEAM, mà Erlang sử dụng, và do đó thừa hưởng các tính năng mạnh mẽ của Erlang, bao gồm cả thao tác hệ thống tệp tin. Không có hỗ trợ bản địa cho các thao tác này, chúng ta phải chuyển sang tương tác với Erlang.

Các phương pháp thay thế để kiểm tra sự tồn tại của thư mục phụ thuộc vào hệ thống cơ sở. Trong các ngôn ngữ lập trình khác, có thể có các lời gọi trực tiếp hoặc thư viện chuẩn có sẵn cho các nhiệm vụ này. Ví dụ, trong Python, bạn sẽ sử dụng `os.path.isdir()`.

Chi tiết thực hiện với hàm `file.read_file_info` trong Erlang cho chúng ta biết nó trả về một tuple bao gồm thông tin tệp nếu thao tác thành công hoặc một tuple lỗi nếu nó thất bại. Những tuples này sau đó có thể được kết hợp mẫu để xác định kết quả. Tuple thành công trông như `{:ok, FileInfo}`, trong khi lỗi được biểu diễn như `{:error, Reason}`.

Mặc dù việc áp dụng các thư viện Erlang trong Gleam hiện là cách tiến lên cho các nhiệm vụ như thế này, đáng chú ý là cộng đồng có thể giới thiệu một gói chuyên dụng cho tương tác hệ thống tệp tin trong tương lai.

## Xem Thêm

- [Tài liệu module 'file' của Erlang](http://erlang.org/doc/man/file.html)
- [Lý do lỗi của Erlang cho thao tác tệp tin](http://erlang.org/doc/apps/stdlib/io_protocol.html)
