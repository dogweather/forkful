---
title:                "Viết một tệp văn bản"
aliases:
- /vi/lua/writing-a-text-file.md
date:                  2024-01-28T22:13:06.812963-07:00
model:                 gpt-4-0125-preview
simple_title:         "Viết một tệp văn bản"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/lua/writing-a-text-file.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Lý do & Tại sao?
Viết một tệp văn bản bao gồm việc lưu dữ liệu vào một tệp dưới dạng có thể đọc được. Các lập trình viên thực hiện điều này để lưu cấu hình, lưu dữ liệu người dùng, hoặc ghi thông tin để gỡ lỗi.

## Cách thực hiện:
```Lua
-- Viết vào một tệp văn bản trong Lua
local fileName = "example.txt"
local content = "Xin chào, tệp tin!"

local file = io.open(fileName, "w") -- Mở tệp ở chế độ viết
if file then
    file:write(content)              -- Viết nội dung vào tệp
    file:close()                     -- Luôn đóng tệp khi đã xong
else
    print("Lỗi khi mở tệp!")
end
```
Kết quả trong `example.txt`:
```
Xin chào, tệp tin!
```

Đọc tệp văn bản:
```Lua
local file = io.open(fileName, "r") -- Mở tệp ở chế độ đọc
if file then
    local fileContent = file:read("*a") -- Đọc toàn bộ nội dung
    print(fileContent)                     -- Xuất nội dung ra console
    file:close()                        -- Đóng tệp
else
    print("Lỗi khi đọc tệp!")
end
```
Xuất ra console:
```
Xin chào, tệp tin!
```

## Sâu hơn
Paradigm xử lý tệp của Lua có nguồn gốc từ thư viện stdio của C, được biết đến với sự đơn giản và linh hoạt của nó. Khác với cơ sở dữ liệu hoặc định dạng nhị phân, tệp văn bản dễ chỉnh sửa và có thể đọc được mà không cần công cụ đặc biệt. Khi xử lý lưu trữ dữ liệu quy mô nhỏ hoặc định dạng dữ liệu đơn giản, tệp văn bản là một lựa chọn phù hợp do tính khả dụng và khả năng tương thích trên nhiều nền tảng của chúng. Về mặt triển khai, thư viện `io` của Lua quản lý các thao tác tệp, bao gồm mở (`io.open`), đọc (`file:read`), viết (`file:write`), và đóng tệp (`file:close`).

## Xem thêm
- Sổ tay tham khảo Lua 5.4: https://www.lua.org/manual/5.4/
- Lập trình Lua (ấn bản thứ 4): https://www.lua.org/pil/contents.html
- So sánh mô hình I/O: https://www.lua.org/pil/21.2.1.html
