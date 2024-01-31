---
title:                "Làm việc với CSV"
date:                  2024-01-28T22:10:22.929675-07:00
model:                 gpt-4-0125-preview
simple_title:         "Làm việc với CSV"

category:             "Lua"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/lua/working-with-csv.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cái Gì và Tại Sao?

Làm việc với CSV (Giá Trị Ngăn Cách Bằng Dấu Phẩy) có nghĩa là phân tích cú pháp và tạo dữ liệu văn bản được phân tách bằng dấu phẩy. Các lập trình viên thực hiện điều này vì sự đơn giản và khả năng tương thích - hầu như mọi hệ thống và ngôn ngữ đều hỗ trợ CSV, làm cho nó trở thành lựa chọn không cần suy nghĩ cho trao đổi dữ liệu.

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
