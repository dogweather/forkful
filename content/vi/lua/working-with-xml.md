---
title:                "Làm việc với XML"
date:                  2024-01-28T22:12:18.913976-07:00
model:                 gpt-4-0125-preview
simple_title:         "Làm việc với XML"
programming_language: "Lua"
category:             "Lua"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/lua/working-with-xml.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Gì và Tại sao?
Làm việc với XML bao gồm việc phân tích cú pháp và thao tác với tài liệu XML sử dụng mã lập trình. Các lập trình viên làm việc này để đọc, viết và sửa đổi dữ liệu trong một định dạng có cấu trúc, dễ di chuyển, được sử dụng rộng rãi cho việc trao đổi và lưu trữ dữ liệu.

## Làm thế nào:
Lua không bao gồm phân tích cú pháp XML gốc, nhưng có các thư viện như LuaXML và xml2lua làm được công việc đó. Dưới đây là cái nhìn nhanh về cách phân tích cú pháp XML với xml2lua:

```Lua
local xml2lua = require("xml2lua")
local handler = require("xmlhandler.tree")

local xmlParser = xml2lua.parser(handler)
xmlParser:parse([[<root><book id="123">Lập trình Lua</book></root>]])

print(handler.root.book._attr.id)  -- Kết quả: 123
print(handler.root.book[1])        -- Kết quả: Lập trình Lua
```

Đối với việc viết XML, dưới đây là một ví dụ nhỏ sử dụng LuaXML:

```Lua
local luaxml = require("LuaXML")

local xml = xml.new("root")
xml:append("book")[1] = "Lập trình Lua"
xml.book._attr = {id="123"}

print(xml:tag())  -- Kết quả: <root><book id="123">Lập trình Lua</book></root>
```

## Sâu hơn
XML, viết tắt của Extensible Markup Language, đã trở thành một tiêu chuẩn trong đại diện dữ liệu và trao đổi dữ liệu kể từ giữa những năm 90. Nó mang lại cấu trúc cho dữ liệu và đồng thời dễ đọc cho con người và máy móc có thể phân tích.

Mặc dù JSON và YAML hiện đang được ưa chuộng vì sự đơn giản của chúng, XML vẫn phổ biến trong nhiều hệ thống doanh nghiệp và hệ thống cũ. Trong Lua, việc xử lý XML gốc không được tích hợp vì Lua được thiết kế để nhỏ và có thể mở rộng thông qua các module.

Các thư viện XML cho Lua, như LuaXML, xml2lua và những thư viện khác, đã lấp đầy khoảng trống này. LuaXML cung cấp một trình đọc và viết XML nhẹ, trong khi xml2lua sử dụng một cách tiếp cận dựa trên sự kiện tương tự như các bộ phân tích SAX. Những thư viện này thường được thực hiện bằng Lua thuần túy để dễ dàng di chuyển, trong khi một số có thể dựa vào C để tăng hiệu suất.

Khi nói đến hiệu suất và sử dụng bộ nhớ, các thư viện XML của Lua có thể không nhanh bằng những thư viện trong các ngôn ngữ có hỗ trợ gốc. Tuy nhiên, cho hầu hết các trường hợp sử dụng trong Lua, đặc biệt là trong phát triển game hoặc viết kịch bản cho hệ thống nhúng, những thư viện này làm tốt công việc mà không làm quá tải hệ thống.

## Xem thêm
- LuaXML trên GitHub: https://github.com/LuaDist/luaxml
- xml2lua trên GitHub: https://github.com/manoelcampos/xml2lua
- Danh sách các thư viện của Lua.org: https://lua-users.org/wiki/LibrariesAndBindings
