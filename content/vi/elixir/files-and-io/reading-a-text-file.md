---
title:                "Đọc một tệp văn bản"
aliases:
- /vi/elixir/reading-a-text-file/
date:                  2024-01-28T22:04:54.435614-07:00
model:                 gpt-4-0125-preview
simple_title:         "Đọc một tệp văn bản"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/elixir/reading-a-text-file.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cái gì & Tại sao?

Đọc một tệp văn bản có nghĩa là đưa dữ liệu từ một tệp vào chương trình của bạn. Lập trình viên làm điều này để xử lý hoặc phân tích nội dung, như đọc cấu hình, phân tích log, hoặc nhập dữ liệu.

## Cách thực hiện:

Dưới đây là cách để đọc toàn bộ nội dung của một tệp văn bản có tên `example.txt`:

```elixir
File.read("example.txt")
```

Mẫu đầu ra nếu `example.txt` chứa "Hello, Elixir!":

```elixir
{:ok, "Hello, Elixir!"}
```

Để đọc tệp theo từng dòng:

```elixir
File.stream!("example.txt")
|> Enum.each(fn line -> IO.puts(line) end)
```

Điều này sẽ in mỗi dòng của `example.txt` ra bảng điều khiển.

## Đào Sâu

Trong Elixir, `File.read/1` và `File.stream!/1` là các cách điển hình để đọc các tệp văn bản. Lịch sử, việc đọc tệp trong lập trình phát triển từ nhu cầu lưu trữ và truy xuất dữ liệu. Trong thời kỳ đầu của máy tính, việc này được thực hiện bằng cách sử dụng thẻ đục lỗ hoặc băng từ. Ngày nay, chúng ta sử dụng các thiết bị lưu trữ khác nhau như SSDs, HDDs, và nhiều hơn nữa.

Một phương án khác cho `File.read/1` là `File.read!/1`, cái mà sẽ báo lỗi nếu có điều gì đó không đúng thay vì trả về một tuple. Tương tự, `File.stream!/1` khác với `File.stream/1` bằng cách báo lỗi khi thất bại thay vì trả về một tuple lỗi.

Cài đặt phía sau cánh gà xử lý dữ liệu nhị phân. Văn bản được chuyển đổi thành nhị phân bởi Elixir, nó xử lý các byte và mã hóa cơ bản.

## Xem Thêm:

- Tài liệu chính thức của module `File` trong Elixir: [https://hexdocs.pm/elixir/File.html](https://hexdocs.pm/elixir/File.html)
