---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:56:17.449571-07:00
description: "L\xE0m th\u1EBF n\xE0o: Module `File` c\u1EE7a Elixir l\xE0 n\u01A1\
  i b\u1EA1n n\xEAn \u0111\u1EBFn \u0111\u1EC3 ki\u1EC3m tra th\u01B0 m\u1EE5c. S\u1EED\
  \ d\u1EE5ng `File.dir?/1` \u0111\u1EC3 tr\u1EA3 v\u1EC1 m\u1ED9t gi\xE1 tr\u1ECB\
  \ boolean ch\u1EC9 ra li\u1EC7u th\u01B0 m\u1EE5c c\xF3\u2026"
lastmod: '2024-03-13T22:44:36.227138-06:00'
model: gpt-4-0125-preview
summary: "Module `File` c\u1EE7a Elixir l\xE0 n\u01A1i b\u1EA1n n\xEAn \u0111\u1EBF\
  n \u0111\u1EC3 ki\u1EC3m tra th\u01B0 m\u1EE5c."
title: "Ki\u1EC3m tra xem th\u01B0 m\u1EE5c c\xF3 t\u1ED3n t\u1EA1i kh\xF4ng"
weight: 20
---

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
