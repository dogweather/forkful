---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:09:25.502988-07:00
description: "REPL vi\u1EBFt t\u1EAFt c\u1EE7a Read-Eval-Print Loop, m\u1ED9t m\xF4\
  i tr\u01B0\u1EDDng t\u01B0\u01A1ng t\xE1c n\u01A1i b\u1EA1n c\xF3 th\u1EC3 nhanh\
  \ ch\xF3ng ki\u1EC3m tra m\xE3. C\xE1c l\u1EADp tr\xECnh vi\xEAn s\u1EED d\u1EE5\
  ng n\xF3 \u0111\u1EC3 th\u1EED nghi\u1EC7m, g\u1EE1\u2026"
lastmod: '2024-03-13T22:44:36.824074-06:00'
model: gpt-4-0125-preview
summary: "REPL vi\u1EBFt t\u1EAFt c\u1EE7a Read-Eval-Print Loop, m\u1ED9t m\xF4i tr\u01B0\
  \u1EDDng t\u01B0\u01A1ng t\xE1c n\u01A1i b\u1EA1n c\xF3 th\u1EC3 nhanh ch\xF3ng\
  \ ki\u1EC3m tra m\xE3."
title: "S\u1EED d\u1EE5ng v\u1ECF t\u01B0\u01A1ng t\xE1c (REPL)"
weight: 34
---

## Cách thực hiện:
Để vào REPL của Lua, chỉ cần nhập `lua` trong terminal của bạn. Dưới đây là một phiên làm việc ví dụ:

```Lua
> x = 10
> print(x * 2)
20
> t = {'apple', 'banana', 'cherry'}
> table.insert(t, 'date')
> for i, fruit in ipairs(t) do print(i, fruit) end
1	apple
2	banana
3	cherry
4	date
>
```
Trong phiên này, chúng ta khai báo một biến, thực hiện toán học cơ bản, thao tác với một bảng và lặp qua các mục của nó.

## Sâu hơn
Bản chất nhẹ của Lua làm cho REPL của nó trở nên lý tưởng cho prototyping. Nó đã có mặt từ khi Lua ra đời vào đầu những năm 1990, được truyền cảm hứng từ các shell tương tác trước đó cho các ngôn ngữ như Lisp. Các lựa chọn khác trong các ngôn ngữ khác bao gồm `irb` cho Ruby và `python` cho Python, mỗi cái có bộ tính năng riêng của mình. REPL của Lua là tối giản; do đó, nó có thể thiếu các tính năng nâng cao có trong những cái khác, như các công cụ gỡ lỗi phức tạp. Để có trải nghiệm mạnh mẽ hơn, các công cụ như ZeroBrane Studio hoặc LuaDist's LuaRocks cung cấp nhiều hơn so với REPL cơ bản.

## Xem thêm
- [Lua 5.4 Reference Manual - The Standalone Lua Interpreter](https://www.lua.org/manual/5.4/manual.html#6)
- [ZeroBrane Studio](https://studio.zerobrane.com/)
- [LuaRocks](https://luarocks.org/)
