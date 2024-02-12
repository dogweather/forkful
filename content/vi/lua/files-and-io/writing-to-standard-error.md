---
title:                "Ghi vào lỗi chuẩn"
aliases:
- /vi/lua/writing-to-standard-error.md
date:                  2024-01-28T22:14:06.539131-07:00
model:                 gpt-4-0125-preview
simple_title:         "Ghi vào lỗi chuẩn"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/lua/writing-to-standard-error.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Làm gì & Tại sao?

Viết vào lỗi chuẩn (stderr) cho phép chương trình của bạn báo cáo về lỗi mà không làm lộn xộn đầu ra chuẩn (stdout). Đây là một tín hiệu rõ ràng cho người dùng và các chương trình khác biết rằng có điều gì đó cần được chú ý.

## Cách thực hiện:

Lua giao tiếp với stderr thông qua `io.stderr`. Dưới đây là cách in một thông báo lỗi đơn giản:

```lua
io.stderr:write("Lỗi: Có sự cố xảy ra!\n")
```

Mẫu đầu ra trên stderr:
```
Lỗi: Có sự cố xảy ra!
```

Bạn có thể trở nên tinh tế hơn và kết hợp nó với xử lý lỗi:

```lua
if not file then
    io.stderr:write("Lỗi: Không tìm thấy tệp.\n")
    os.exit(1) -- thoát với mã lỗi không bằng không
end
```

## Sâu hơn

Từ lâu, máy tính có hai dòng đầu ra tách biệt—stdout cho dữ liệu chính, stderr cho những sự cố. Lua giữ nguyên quy ước Unix này. Đôi khi, mọi người chuyển hướng stdout (như sang một tệp) nhưng vẫn muốn lỗi xuất hiện trên màn hình. Đó là lúc stderr được sử dụng.

Có phương án thay thế không? Một số người ghi vào một tệp nhật ký, sử dụng thư viện ghi nhật ký, hoặc gửi qua mạng. Nhưng stderr thấp cản trở cho những việc đơn giản.

Về mặt triển khai, `io.stderr` của Lua là một tay cầm tệp. Nó giống như `io.stdout` hay `io.stdin`, sẵn sàng hoạt động mà không cần rắc rối. Phía sau hậu trường, không quan trọng nó là một tệp văn bản hay một terminal, Lua không lo lắng—`io.stderr` xử lý nó.

## Xem thêm

Khám phá sâu hơn hoặc hiểu rõ hơn:

- Sổ tay tham chiếu Lua 5.4: http://www.lua.org/manual/5.4/
- Triết lý Unix: https://en.wikipedia.org/wiki/Unix_philosophy
- Tìm hiểu thêm về `os.exit`: http://www.lua.org/pil/21.3.html
- Một chuyến tham quan về cơ sở đầu vào và đầu ra của Lua: http://www.lua.org/pil/21.html
