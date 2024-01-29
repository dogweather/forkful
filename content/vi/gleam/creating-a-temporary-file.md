---
title:                "Tạo một tập tin tạm thời"
date:                  2024-01-28T21:58:58.133554-07:00
model:                 gpt-4-0125-preview
simple_title:         "Tạo một tập tin tạm thời"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/gleam/creating-a-temporary-file.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cái gì & Tại sao?

Việc tạo một tệp tin tạm thời chính xác là như cái tên của nó - tạo một tệp tin để sử dụng trong thời gian ngắn. Các lập trình viên thực hiện việc này cho các công việc như lưu trữ dữ liệu tạm thời hoặc khi họ cần làm việc với một tệp tin mà không ảnh hưởng đến nội dung gốc.

## Cách thực hiện:

Gleam vẫn chưa có thư viện tiêu chuẩn dành riêng cho các thao tác với tệp tin. Thay vào đó, chúng ta phụ thuộc vào Erlang interop để thực hiện các nhiệm vụ như tạo tệp tin tạm thời. Dưới đây là cách bạn thực hiện:

```gleam
import gleam/erlang

fn main() {
  // Sử dụng mô-đun `:file` của Erlang để tạo tệp tin tạm thời
  case erlang.apply(
    module: ":file",
    function: "mktemp",
    arguments: ["./tmpFileXXXXXX"]
  ) {
    Ok(tuple) -> 
      let tmpPath = tuple.element(2)
      erlang.display(tmpPath) // Đường dẫn nơi tệp tin tạm thời được tạo
    Error(error_tuple) ->
      erlang.display(error_tuple)
  }
}
```

Khi bạn chạy chương trình, bạn sẽ nhận được đầu ra tương tự như thế này:

```plain
./tmpFileab3d5f
```

Đó là đường dẫn đến tệp tin tạm thời mới toanh của bạn.

## Sâu hơn nữa

Truyền thống, việc làm việc trực tiếp với tệp tin trong Gleam là việc tận dụng khả năng cơ bản của Erlang, mà Gleam được biên dịch thành. Có thể có các gói Gleam bên thứ ba ngoài kia cung cấp một cách thức idiom hơn, có lẽ là an toàn hơn để xử lý tệp tin, nhưng cái chính là từ mô-đun `:file` đã được thử nghiệm và kiểm định của Erlang.

Các phương án thay thế bao gồm sử dụng khả năng của hệ điều hành qua các lệnh shell, nhưng điều này có thể kém linh hoạt hơn và khó triển khai theo cách tương thích với nhiều nền tảng. Sự thoải mái trong ngôn ngữ có thể được cải thiện khi hệ sinh thái phát triển - vì vậy hãy chú ý đến các thư viện nổi lên!

Tệp tin tạm thời là không thể thiếu cho các kịch bản khi bạn không muốn lưu trữ dữ liệu lâu hơn mức cần thiết, và chúng cũng giúp ngăn chặn sự hỏng hóc dữ liệu trong các thao tác rủi ro. Khi tạo một tệp tạm, thường sẽ tạo một tên duy nhất để giảm thiểu nguy cơ va chạm tên, và việc này được thực hiện trong một thư mục dành cho lưu trữ tạm thời đảm bảo nó sẽ được dọn dẹp sau này, bởi chương trình của bạn hoặc bởi OS.

## Xem thêm

Để biết thông tin chi tiết hơn về việc xử lý tệp tin trong Gleam, hãy xem những liên kết sau:

- [Tài liệu mô-đun :file của Erlang](http://erlang.org/doc/man/file.html)

Nhớ rằng, Gleam vẫn còn khá mới trong thế giới lập trình, nghĩa là hệ sinh thái của nó đang phát triển. Hãy theo dõi [Hex](https://hex.pm/) (trình quản lý gói cho hệ sinh thái Erlang) để biết các gói xử lý tệp tin trong tương lai có thể xuất hiện!
