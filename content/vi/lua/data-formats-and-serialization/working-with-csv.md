---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:10:22.929675-07:00
description: "L\xE0m Th\u1EBF N\xE0o: H\xE3y \u0111\u1ECDc v\xE0 vi\u1EBFt c\xE1c\
  \ t\u1EC7p CSV b\u1EB1ng Lua. Ch\xFAng t\xF4i s\u1EBD x\u1EED l\xFD m\u1ED9t v\xED\
  \ d\u1EE5 c\u01A1 b\u1EA3n m\xE0 kh\xF4ng c\u1EA7n th\u01B0 vi\u1EC7n b\xEAn ngo\xE0\
  i. **\u0110\u1ECDc m\u1ED9t t\u1EC7p CSV:**."
lastmod: '2024-03-13T22:44:36.850915-06:00'
model: gpt-4-0125-preview
summary: "H\xE3y \u0111\u1ECDc v\xE0 vi\u1EBFt c\xE1c t\u1EC7p CSV b\u1EB1ng Lua."
title: "L\xE0m vi\u1EC7c v\u1EDBi CSV"
weight: 37
---

## Làm Thế Nào:
Hãy đọc và viết các tệp CSV bằng Lua. Chúng tôi sẽ xử lý một ví dụ cơ bản mà không cần thư viện bên ngoài.

**Đọc một tệp CSV:**

```Lua
function read_csv(filepath)
  local results = {}
  local file = assert(io.open(filepath, "r"))
  
  for line in file:lines() do
    table.insert(results, line:split(","))
  end
  
  file:close()
  return results
end

-- Một hàm trợ giúp để tách chuỗi
function string:split(delimiter)
  local result = {}
  local from = 1
  local delim_from, delim_to = self:find(delimiter, from, true)
  while delim_from do
    table.insert(result, self:sub(from, delim_from - 1))
    from = delim_to + 1
    delim_from, delim_to = self:find(delimiter, from, true)
  end
  table.insert(result, self:sub(from))
  return result
end
```

**Viết vào một tệp CSV:**

```Lua
function write_csv(filepath, data)
  local file = assert(io.open(filepath, "w"))
  
  for _, row in ipairs(data) do
    file:write(table.concat(row, ",") .. "\n")
  end
  
  file:close()
end

-- Dữ liệu mẫu
local data = {
  { "Tên", "Tuổi", "Thành Phố" },
  { "Alice", "30", "New York" },
  { "Bob", "25", "Los Angeles" }
}

write_csv("output.csv", data)
```

## Sâu Hơn
Lịch sử của CSV có từ những ngày đầu của công nghệ thông tin, nơi sự đơn giản là chìa khóa. Mặc dù JSON và XML hiện cung cấp các cấu trúc dữ liệu phong phú hơn, CSV vẫn được ưa chuộng do khả năng đọc và dễ dàng chỉnh sửa bằng phần mềm bảng tính. Khi triển khai, hãy chú ý đến các trường có dấu phẩy, dòng mới hoặc dấu nháy – những trường đó nên được đặt trong dấu nháy và/hoặc được thoát ra một cách thích hợp.

## Xem Thêm
- Sổ tay tham khảo chính thức Lua 5.4: https://www.lua.org/manual/5.4/
- RFC 4180, Định dạng chung và loại MIME cho các tệp Giá Trị Ngăn Cách bởi Dấu Phẩy (CSV): https://tools.ietf.org/html/rfc4180
- Thư viện Lua Penlight (cho việc xử lý CSV tiên tiến hơn): https://github.com/lunarmodules/Penlight
