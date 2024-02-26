---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:04:54.435614-07:00
description: "\u0110\u1ECDc m\u1ED9t t\u1EC7p v\u0103n b\u1EA3n c\xF3 ngh\u0129a l\xE0\
  \ \u0111\u01B0a d\u1EEF li\u1EC7u t\u1EEB m\u1ED9t t\u1EC7p v\xE0o ch\u01B0\u01A1\
  ng tr\xECnh c\u1EE7a b\u1EA1n. L\u1EADp tr\xECnh vi\xEAn l\xE0m \u0111i\u1EC1u n\xE0\
  y \u0111\u1EC3 x\u1EED l\xFD ho\u1EB7c ph\xE2n t\xEDch n\u1ED9i dung, nh\u01B0 \u0111\
  \u1ECDc c\u1EA5u\u2026"
lastmod: '2024-02-25T18:49:34.611451-07:00'
model: gpt-4-0125-preview
summary: "\u0110\u1ECDc m\u1ED9t t\u1EC7p v\u0103n b\u1EA3n c\xF3 ngh\u0129a l\xE0\
  \ \u0111\u01B0a d\u1EEF li\u1EC7u t\u1EEB m\u1ED9t t\u1EC7p v\xE0o ch\u01B0\u01A1\
  ng tr\xECnh c\u1EE7a b\u1EA1n. L\u1EADp tr\xECnh vi\xEAn l\xE0m \u0111i\u1EC1u n\xE0\
  y \u0111\u1EC3 x\u1EED l\xFD ho\u1EB7c ph\xE2n t\xEDch n\u1ED9i dung, nh\u01B0 \u0111\
  \u1ECDc c\u1EA5u\u2026"
title: "\u0110\u1ECDc m\u1ED9t t\u1EC7p v\u0103n b\u1EA3n"
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
