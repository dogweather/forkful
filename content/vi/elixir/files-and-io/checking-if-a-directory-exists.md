---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:56:17.449571-07:00
description: "Vi\u1EC7c ki\u1EC3m tra xem m\u1ED9t th\u01B0 m\u1EE5c c\xF3 t\u1ED3\
  n t\u1EA1i trong Elixir \u0111\u1EA3m b\u1EA3o b\u1EA1n \u0111ang t\u01B0\u01A1\
  ng t\xE1c v\u1EDBi m\u1ED9t \u0111\u01B0\u1EDDng d\u1EABn t\u1EC7p h\u1EE3p l\u1EC7\
  . L\u1EADp tr\xECnh vi\xEAn l\xE0m \u0111i\u1EC1u n\xE0y \u0111\u1EC3 tr\xE1nh c\xE1\
  c\u2026"
lastmod: '2024-02-25T18:49:34.607554-07:00'
model: gpt-4-0125-preview
summary: "Vi\u1EC7c ki\u1EC3m tra xem m\u1ED9t th\u01B0 m\u1EE5c c\xF3 t\u1ED3n t\u1EA1\
  i trong Elixir \u0111\u1EA3m b\u1EA3o b\u1EA1n \u0111ang t\u01B0\u01A1ng t\xE1c\
  \ v\u1EDBi m\u1ED9t \u0111\u01B0\u1EDDng d\u1EABn t\u1EC7p h\u1EE3p l\u1EC7. L\u1EAD\
  p tr\xECnh vi\xEAn l\xE0m \u0111i\u1EC1u n\xE0y \u0111\u1EC3 tr\xE1nh c\xE1c\u2026"
title: "Ki\u1EC3m tra xem th\u01B0 m\u1EE5c c\xF3 t\u1ED3n t\u1EA1i kh\xF4ng"
---

{{< edit_this_page >}}

## Gì và Tại sao?

Việc kiểm tra xem một thư mục có tồn tại trong Elixir đảm bảo bạn đang tương tác với một đường dẫn tệp hợp lệ. Lập trình viên làm điều này để tránh các lỗi như cố gắng đọc từ hoặc ghi vào một vị trí không tồn tại, có thể làm sập ứng dụng của họ hoặc phá vỡ một quy trình.

## Làm thế nào:

Module `File` của Elixir là nơi bạn nên đến để kiểm tra thư mục. Sử dụng `File.dir?/1` để trả về một giá trị boolean chỉ ra liệu thư mục có tồn tại hay không.

```elixir
# Kiểm tra xem thư mục có tồn tại không
if File.dir?("/đường/dẫn/tới/thư_mục") do
  IO.puts("Thư mục tồn tại!")
else
  IO.puts("Không có thư mục nào như vậy.")
end
```

Đầu ra mẫu cho một thư mục tồn tại:
```elixir
Thư mục tồn tại!
```

Đầu ra mẫu cho một thư mục không tồn tại:
```elixir
Không có thư mục nào như vậy.
```

## Khám Phá Sâu

Theo lịch sử, các hoạt động hệ thống tệp đã mang lại ý nghĩa quan trọng trong lập trình do nhu cầu đọc/ghi dữ liệu. Trong Elixir, module `File` đã trừu tượng hoá các hoạt động này một cách gọn gàng. Điều đáng nói ở đây là sự tin cậy với các kiểm tra này; do đó, `File.dir?/1` là một công cụ chính yếu để xác minh các đường dẫn.

Các phương pháp thay thế cho `File.dir?/1` có thể là sử dụng `File.stat/2` và kiểm tra xem kết quả có phải là `:ok` không, điều này chỉ ra rằng thư mục tồn tại. Một cách tiếp cận khác có thể là sử dụng `:filelib.is_dir/1` từ thư viện chuẩn của Erlang, mà Elixir có thể truy cập do tính tương tác của nó với Erlang.

Việc thực hiện kiểm tra xem một thư mục có tồn tại không của Elixir được xây dựng trên nền tảng xử lý tệp vững chắc của Erlang. Thiết kế này tận dụng khả năng của BEAM để tạo ra các hệ thống chịu lỗi, nơi các ứng dụng Elixir thường chạy.

## Xem Thêm

- Tài liệu module `File` của Elixir: [https://hexdocs.pm/elixir/File.html](https://hexdocs.pm/elixir/File.html)
- Tài liệu module `filelib` của Erlang cho nhiều chức năng hệ thống tệp hơn: [http://erlang.org/doc/man/filelib.html](http://erlang.org/doc/man/filelib.html)
- Xử lý tệp vững chắc trong Elixir: [https://elixir-lang.org/getting-started/io-and-the-file-system.html](https://elixir-lang.org/getting-started/io-and-the-file-system.html)
