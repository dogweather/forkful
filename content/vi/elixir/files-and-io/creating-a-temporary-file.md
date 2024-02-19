---
aliases:
- /vi/elixir/creating-a-temporary-file/
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:58:41.538253-07:00
description: "T\u1EA1o m\u1ED9t t\u1EC7p t\u1EA1m th\u1EDDi c\xF3 ngh\u0129a l\xE0\
  \ t\u1EA1o m\u1ED9t t\u1EC7p b\u1EA1n s\u1EBD lo\u1EA1i b\u1ECF sau m\u1ED9t th\u1EDD\
  i gian s\u1EED d\u1EE5ng ng\u1EAFn. C\xE1c l\u1EADp tr\xECnh vi\xEAn th\u1EF1c hi\u1EC7\
  n \u0111i\u1EC1u n\xE0y cho vi\u1EC7c l\u01B0u tr\u1EEF t\u1EA1m\u2026"
lastmod: 2024-02-18 23:08:50.390308
model: gpt-4-0125-preview
summary: "T\u1EA1o m\u1ED9t t\u1EC7p t\u1EA1m th\u1EDDi c\xF3 ngh\u0129a l\xE0 t\u1EA1\
  o m\u1ED9t t\u1EC7p b\u1EA1n s\u1EBD lo\u1EA1i b\u1ECF sau m\u1ED9t th\u1EDDi gian\
  \ s\u1EED d\u1EE5ng ng\u1EAFn. C\xE1c l\u1EADp tr\xECnh vi\xEAn th\u1EF1c hi\u1EC7\
  n \u0111i\u1EC1u n\xE0y cho vi\u1EC7c l\u01B0u tr\u1EEF t\u1EA1m\u2026"
title: "T\u1EA1o m\u1ED9t t\u1EADp tin t\u1EA1m th\u1EDDi"
---

{{< edit_this_page >}}

## Cái gì & Tại sao?
Tạo một tệp tạm thời có nghĩa là tạo một tệp bạn sẽ loại bỏ sau một thời gian sử dụng ngắn. Các lập trình viên thực hiện điều này cho việc lưu trữ tạm thời hoặc khi họ muốn tránh làm đầy ổ cứng với dữ liệu có tuổi thọ rất ngắn.

## Cách thực hiện:
Trong Elixir, bạn có thể tạo và sử dụng một tệp tạm thời với hàm `System.tmp_dir/1` và module `File`. Dưới đây là một ví dụ nhanh:

```elixir
# Hãy lăn tay áo và bắt tay vào công việc!

# Tìm thư mục temp
temp_dir = System.tmp_dir!()

# Tạo đường dẫn tệp tạm thời
temp_file_path = Path.join(temp_dir, "my_temp_file.txt")

# Hãy viết điều gì đó tạm thời
File.write!(temp_file_path, "Xin chào, thế giới tạm thời!")

# Đọc nó, chỉ để đảm bảo mọi thứ đều ổn
IO.puts(File.read!(temp_file_path))

# Dọn dẹp sau khi sử dụng và xóa tệp tạm thời
File.rm!(temp_file_path)
```

Kết quả mẫu:
```
Xin chào, thế giới tạm thời!
```

## Đi sâu hơn
Tệp tạm thời không chỉ độc quyền trong Elixir. Chúng là một yếu tố quan trọng trong các ngôn ngữ lập trình vì chúng hoàn hảo cho việc xử lý dữ liệu chỉ quan trọng trong thời gian thực thi chương trình. Trước khi lưu trữ trở nên rẻ, việc tiết kiệm không gian đĩa là rất quan trọng - tệp tạm thời đã giúp ích cho việc đó. Ngày nay, chúng hữu ích cho việc quản lý tài nguyên và bảo mật: dữ liệu ít lâu dài hơn có nghĩa là để lại ít dấu vết hơn.

Đối với các lựa chọn thay thế, trong Elixir, bạn có thể tự lập trình logic tệp tạm thời của mình hoặc sử dụng trực tiếp các hàm Erlang (ví dụ: `:erlang.mktemp/0`). Và về chi tiết, khi bạn tạo một tệp tạm thời, các chi tiết như việc đặt tên được xử lý bởi hệ điều hành của bạn, không phải bởi chính Elixir. Elixir chỉ yêu cầu hệ điều hành nơi để lưu trữ tệp một cách tạm thời, và hệ điều hành trả lời.

## Xem thêm
Để tìm hiểu thêm về thao tác tệp trong Elixir:
- Module `File` của Elixir: https://hexdocs.pm/elixir/File.html
- Tài liệu chính thức cho `System.tmp_dir/1`: https://hexdocs.pm/elixir/System.html#tmp_dir/1

Khám phá khả năng quản lý tệp của Erlang:
- Module `file` của Erlang: http://erlang.org/doc/man/file.html
