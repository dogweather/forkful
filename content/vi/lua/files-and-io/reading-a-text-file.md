---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:05:21.455486-07:00
description: "L\xE0m th\u1EBF n\xE0o: H\xE3y xem c\xE1ch \u0111\u1ECDc m\u1ED9t t\u1EC7\
  p v\u0103n b\u1EA3n d\xF2ng tr\xEAn d\xF2ng v\xE0 sau \u0111\xF3 l\xE0 \u0111\u1ECD\
  c to\xE0n b\u1ED9 m\u1ED9t l\u1EA7n."
lastmod: '2024-03-13T22:44:36.844508-06:00'
model: gpt-4-0125-preview
summary: "H\xE3y xem c\xE1ch \u0111\u1ECDc m\u1ED9t t\u1EC7p v\u0103n b\u1EA3n d\xF2\
  ng tr\xEAn d\xF2ng v\xE0 sau \u0111\xF3 l\xE0 \u0111\u1ECDc to\xE0n b\u1ED9 m\u1ED9\
  t l\u1EA7n."
title: "\u0110\u1ECDc m\u1ED9t t\u1EC7p v\u0103n b\u1EA3n"
weight: 22
---

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
