---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:05:21.455486-07:00
description: "\u0110\u1ECDc m\u1ED9t t\u1EC7p v\u0103n b\u1EA3n c\xF3 ngh\u0129a l\xE0\
  \ t\u1EA3i n\u1ED9i dung c\u1EE7a n\xF3 v\xE0o ch\u01B0\u01A1ng tr\xECnh c\u1EE7\
  a b\u1EA1n. Ch\xFAng ta th\u1EF1c hi\u1EC7n \u0111i\u1EC1u n\xE0y \u0111\u1EC3 x\u1EED\
  \ l\xFD, ph\xE2n t\xEDch, ho\u1EB7c hi\u1EC3n th\u1ECB d\u1EEF li\u1EC7u \u0111\xE3\
  \u2026"
lastmod: '2024-02-25T18:49:35.188015-07:00'
model: gpt-4-0125-preview
summary: "\u0110\u1ECDc m\u1ED9t t\u1EC7p v\u0103n b\u1EA3n c\xF3 ngh\u0129a l\xE0\
  \ t\u1EA3i n\u1ED9i dung c\u1EE7a n\xF3 v\xE0o ch\u01B0\u01A1ng tr\xECnh c\u1EE7\
  a b\u1EA1n. Ch\xFAng ta th\u1EF1c hi\u1EC7n \u0111i\u1EC1u n\xE0y \u0111\u1EC3 x\u1EED\
  \ l\xFD, ph\xE2n t\xEDch, ho\u1EB7c hi\u1EC3n th\u1ECB d\u1EEF li\u1EC7u \u0111\xE3\
  \u2026"
title: "\u0110\u1ECDc m\u1ED9t t\u1EC7p v\u0103n b\u1EA3n"
---

{{< edit_this_page >}}

## Gì & Tại Sao?

Đọc một tệp văn bản có nghĩa là tải nội dung của nó vào chương trình của bạn. Chúng ta thực hiện điều này để xử lý, phân tích, hoặc hiển thị dữ liệu đã lưu, như cài đặt, nhật ký, hoặc nhập liệu của người dùng.

## Làm thế nào:

Hãy xem cách đọc một tệp văn bản dòng trên dòng và sau đó là đọc toàn bộ một lần.

```Lua
-- Đọc tệp dòng theo dòng
local file = io.open("example.txt", "r") -- Mở tệp để đọc
if file then
  for line in file:lines() do -- Lặp qua từng dòng trong tệp
    print(line)
  end
  file:close() -- Luôn đóng tệp khi bạn hoàn thành
else
  print("Không thể mở tệp.")
end

-- Đọc toàn bộ tệp cùng một lúc
local file = io.open("example.txt", "r") -- Mở tệp để đọc
if file then
  local content = file:read("*a") -- Đọc toàn bộ nội dung
  print(content)
  file:close() -- Đóng tệp
else
  print("Không thể mở tệp.")
end
```

Kết quả mẫu cho cả hai đoạn mã, nếu `example.txt` chứa:
```
Hello, Lua!
```

Kết quả sẽ là:
```
Hello, Lua!
```

## Sâu hơn

Theo lịch sử, việc đọc tệp là một hoạt động cơ bản, bắt nguồn từ những máy tính đầu tiên. Trong Lua, điều này được xử lý thông qua mô hình I/O đơn giản với thư viện `io`.

Mặc dù `io.lines` và `io.read` là những cách phổ biến để truy cập vào nội dung của một tệp, nhưng cũng có những phương án thay thế như `lfs` (LuaFileSystem) cho các thao tác tệp tiên tiến.

Khi đọc, Lua xử lý việc đệm ngầm, tuy nhiên đối với những tệp lớn, bạn nên đọc từng phần để tránh việc sử dụng quá nhiều bộ nhớ.

Sử dụng thư viện `io` là điều dễ dàng, nhưng luôn nhớ đóng tệp để tránh rò rỉ tài nguyên. Trong trường hợp lỗi, các thao tác tệp của Lua trả về `nil` và một thông báo lỗi, điều mà bạn nên xử lý để đảm bảo tính mạnh mẽ.

## Xem Thêm:

- [Tài liệu tham khảo Lua 5.4: I/O](https://www.lua.org/manual/5.4/manual.html#6.8)
- [Học Lua](https://learnxinyminutes.com/docs/lua/)
