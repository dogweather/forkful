---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:08:44.188049-07:00
description: '#'
lastmod: '2024-02-25T18:49:35.165131-07:00'
model: gpt-4-0125-preview
summary: '#'
title: "G\u1EEDi m\u1ED9t y\xEAu c\u1EA7u HTTP v\u1EDBi x\xE1c th\u1EF1c c\u01A1 b\u1EA3\
  n"
---

{{< edit_this_page >}}

## Gửi một yêu cầu HTTP với xác thực cơ bản bằng Lua

### Gì và Tại sao?

Gửi một yêu cầu HTTP với xác thực cơ bản là khi bạn thực hiện một cuộc gọi đến một máy chủ web, bao gồm cả tên đăng nhập và mật khẩu để truy cập. Các lập trình viên thực hiện điều này để tương tác với các dịch vụ web yêu cầu xác minh người dùng trước khi cung cấp dữ liệu hoặc dịch vụ.

### Cách thực hiện:

Lua không hỗ trợ HTTP mặc định, vì vậy bạn cần một thư viện bên ngoài như `socket.http` từ LuaSocket hoặc `http.request` từ thư viện `http` nếu bạn sử dụng Lua 5.3 trở lên. Đối với xác thực cơ bản, mã hóa thông tin xác thực và thêm chúng vào tiêu đề yêu cầu.

```Lua
local http = require("socket.http")
local ltn12 = require("ltn12")
local mime = require("mime")

-- Thông tin xác thực của bạn
local username = "Aladdin"
local password = "openSesame"
local credentials = mime.b64(username .. ":" .. password)

-- Thiết lập yêu cầu
local response_body = {}
local res, code, response_headers = http.request{
    url = "http://example.com/data",
    method = "GET",
    headers = {
        ["Authorization"] = "Basic " .. credentials
    },
    sink = ltn12.sink.table(response_body)
}

-- Xuất kết quả
if code == 200 then
    print(table.concat(response_body))
else
    print("Lỗi: " .. (res or code))
end
```

### Tìm hiểu kỹ hơn

Xác thực Cơ bản HTTP là một phương pháp cho một đại lý người dùng HTTP cung cấp tên người dùng và mật khẩu khi thực hiện một yêu cầu. Được phát minh từ những ngày đầu của lịch sử web, nó được hỗ trợ rộng rãi nhưng không rất an toàn; thông tin xác thực chỉ được mã hóa dạng base64, không được mã hóa.

Các phương án thay thế bao gồm Xác thực Hóa học, OAuth, và các khóa API – tất cả đều cung cấp bảo mật mạnh mẽ hơn. Xác thực cơ bản thường được sử dụng để viết kịch bản kiểm tra nhanh, công cụ nội bộ, hoặc trong trường hợp việc vận chuyển được bảo vệ qua HTTPS.

Để thực hiện xác thực cơ bản trong Lua, bạn thường xây dựng một chuỗi kết hợp tên người dùng và mật khẩu được phân tách bằng dấu hai chấm, sau đó mã hóa chuỗi đó bằng base64. Chuỗi đã mã hóa này được gửi trong tiêu đề `Authorization` của yêu cầu HTTP của bạn.

Bản chất linh hoạt của Lua nghĩa là bạn có sự lựa chọn về các thư viện để xử lý HTTP và mã hóa base64. LuaSocket đã là lựa chọn hàng đầu cho các hoạt động mạng trong một thời gian dài, mặc dù các phiên bản mới hơn của Lua giới thiệu các lựa chọn thay thế như thư viện `http` hoặc liên kết `CURL` cho các tác vụ phức tạp hơn.

### Xem thêm

- Tài liệu LuaSocket: http://w3.impa.br/~diego/software/luasocket/http.html
- LuaSec cho hỗ trợ HTTPS: https://github.com/brunoos/luasec/wiki
- Giới thiệu về Xác thực HTTP: https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication
- RFC 2617 – Xác thực HTTP: Xác thực Truy cập Cơ bản và Hóa học: https://tools.ietf.org/html/rfc2617
