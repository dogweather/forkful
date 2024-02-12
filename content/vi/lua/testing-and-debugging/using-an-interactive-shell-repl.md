---
title:                "Sử dụng vỏ tương tác (REPL)"
aliases:
- vi/lua/using-an-interactive-shell-repl.md
date:                  2024-01-28T22:09:25.502988-07:00
model:                 gpt-4-0125-preview
simple_title:         "Sử dụng vỏ tương tác (REPL)"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/lua/using-an-interactive-shell-repl.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Gì & Tại sao?
REPL viết tắt của Read-Eval-Print Loop, một môi trường tương tác nơi bạn có thể nhanh chóng kiểm tra mã. Các lập trình viên sử dụng nó để thử nghiệm, gỡ lỗi và học những điểm đặc biệt của ngôn ngữ.

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
