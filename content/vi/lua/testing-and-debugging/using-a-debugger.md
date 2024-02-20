---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:09:23.519645-07:00
description: "Debugger l\xE0 m\u1ED9t c\xF4ng c\u1EE5 cho ph\xE9p b\u1EA1n ki\u1EC3\
  m tra v\xE0 ki\u1EC3m so\xE1t vi\u1EC7c th\u1EF1c thi c\u1EE7a m\u1ED9t ch\u01B0\
  \u01A1ng tr\xECnh, gi\xFAp d\u1EC5 d\xE0ng x\xE1c \u0111\u1ECBnh v\u1EA5n \u0111\
  \u1EC1 x\u1EA3y ra \u1EDF \u0111\xE2u. L\u1EADp tr\xECnh vi\xEAn\u2026"
lastmod: 2024-02-19 22:04:56.013590
model: gpt-4-0125-preview
summary: "Debugger l\xE0 m\u1ED9t c\xF4ng c\u1EE5 cho ph\xE9p b\u1EA1n ki\u1EC3m tra\
  \ v\xE0 ki\u1EC3m so\xE1t vi\u1EC7c th\u1EF1c thi c\u1EE7a m\u1ED9t ch\u01B0\u01A1\
  ng tr\xECnh, gi\xFAp d\u1EC5 d\xE0ng x\xE1c \u0111\u1ECBnh v\u1EA5n \u0111\u1EC1\
  \ x\u1EA3y ra \u1EDF \u0111\xE2u. L\u1EADp tr\xECnh vi\xEAn\u2026"
title: "S\u1EED d\u1EE5ng b\u1ED9 g\u1EE1 l\u1ED7i"
---

{{< edit_this_page >}}

## Cái gì & Tại sao?
Debugger là một công cụ cho phép bạn kiểm tra và kiểm soát việc thực thi của một chương trình, giúp dễ dàng xác định vấn đề xảy ra ở đâu. Lập trình viên sử dụng debugger để loại bỏ lỗi, hiểu dòng chảy của mã, và đảm bảo rằng mã của họ sạch sẽ như còi săn.

## Làm thế nào:
Lua không đi kèm với debugger tích hợp sẵn, nhưng bạn có thể sử dụng các debugger bên ngoài, như ZeroBrane Studio. Dưới đây là một ví dụ về cách bạn làm việc với nó:

```Lua
-- Đây là một script Lua đơn giản với một lỗi cố ý
local function add(a, b)
    local result = a+ b -- Ôi, hãy giả vờ chúng ta quên định nghĩa 'b'
    return result
end

print(add(10))
```

Khi bạn chạy điều này trong debugger, nó sẽ dừng thực thi ở nơi mọi thứ lộn xộn. Bạn sẽ nhìn thấy điều gì đó như thế này:

```
lua: example.lua:3: attempt to perform arithmetic on a nil value (local 'b')
stack traceback:
	example.lua:3: in function 'add'
	example.lua:7: in main chunk
	[C]: in ?
```

Bạn có thể đặt các điểm dừng, bước qua mã của mình, và xem giá trị của các biến để tìm ra lỗi mà không mất trí.

## Sâu hơn
Sự đơn giản của Lua không mở rộng đến việc gỡ lỗi, rất tiếc. Tuy nhiên, không cần lo lắng, cộng đồng Lua đã hỗ trợ bạn. Các công cụ như ZeroBrane Studio, LuaDec, và những công cụ khác cung cấp khả năng gỡ lỗi. Về mặt lịch sử, debugger đã tồn tại không lâu sau khi các chương trình đầu tiên bị hỏng, cung cấp cho các nhà phát triển phương tiện để sửa chữa mã của họ mà không cần phải mù quáng mò mẫm.

Với Lua, bạn thường phải dựa vào các debugger bên ngoài hoặc tích hợp chúng vào môi trường phát triển của mình. ZeroBrane Studio, ví dụ, là một IDE tích hợp sẵn debugger Lua. Nó cho phép bạn bước qua mã, đặt điểm dừng và xem các biến. Về phần thực thi, debugger thường sử dụng các móc để chèn điểm dừng và các tiện ích gỡ lỗi khác.

Các phương pháp thay thế? Chắc chắn rồi. Câu lệnh `print` cũ, được biết đến với tình yêu "printf debugging," đôi khi cũng có thể giải quyết vấn đề mà không cần đến những công cụ sang trọng.

## Xem thêm
Để tiếp tục hành trình gỡ lỗi của bạn, hãy kiểm tra:

- ZeroBrane Studio: https://studio.zerobrane.com/
- Lua-users wiki về Gỡ lỗi Mã Lua: http://lua-users.org/wiki/DebuggingLuaCode
- Tham khảo thư viện `debug` trong tài liệu Lua: https://www.lua.org/manual/5.4/manual.html#6.10
