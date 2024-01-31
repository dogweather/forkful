---
title:                "Tái cấu trúc mã"
date:                  2024-01-28T22:06:29.744015-07:00
model:                 gpt-4-0125-preview
simple_title:         "Tái cấu trúc mã"

category:             "Lua"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/lua/refactoring.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Gì và Tại sao?
Tái cấu trúc là nghệ thuật chỉnh sửa mã code hiện có để cải thiện cấu trúc, tính dễ đọc và hiệu quả mà không thay đổi hành vi bên ngoài của nó. Lập trình viên thực hiện việc này để làm cho mã của họ dễ bảo trì hơn, giảm độ phức tạp, và thường là bước chuẩn bị trước khi thêm tính năng mới hoặc sửa lỗi.

## Làm thế nào:
Hãy lấy một hàm Lua đơn giản và tái cấu trúc nó. Chúng ta bắt đầu với một hàm tính tổng của các số trong một danh sách nhưng được viết mà không chú trọng nhiều đến hiệu quả hoặc rõ ràng:

```Lua
function sumList(numbers)
    local result = 0
    for i=1, #numbers do
        for j=1, #numbers do
            if i == j then
                result = result + numbers[i]
            end
        end
    end
    return result
end

print(sumList({1, 2, 3, 4})) -- Đầu ra: 10
```

Tái cấu trúc thành phiên bản hiệu quả và dễ đọc hơn:
```Lua
function sumListRefactored(numbers)
    local result = 0
    for _, value in ipairs(numbers) do
        result = result + value
    end
    return result
end

print(sumListRefactored({1, 2, 3, 4})) -- Vẫn đầu ra: 10
```

Phiên bản tái cấu trúc loại bỏ vòng lặp nội bộ dư thừa, sử dụng `ipairs` để lặp qua danh sách một cách sạch sẽ.

## Sâu hơn
Lịch sử, tái cấu trúc bắt nguồn từ cộng đồng lập trình Smalltalk vào cuối những năm 80 và được phổ biến bởi cuốn sách của Martin Fowler 'Refactoring: Improving the Design of Existing Code'. Trong Lua, tái cấu trúc thường liên quan đến việc đơn giản hóa các điều kiện phức tạp, chia nhỏ các hàm lớn thành các hàm nhỏ hơn, và tối ưu hóa việc sử dụng bảng để cải thiện hiệu suất.

Tái cấu trúc trong Lua có những cảnh báo của nó; bản chất động và kiểu dữ liệu linh hoạt của Lua có thể làm cho một số tái cấu trúc, như đổi tên biến hoặc thay đổi chữ ký hàm, rủi ro hơn nếu không thực hiện cẩn thận. Các công cụ phân tích mã tĩnh (như `luacheck`) có thể giảm bớt những rủi ro như vậy. Các phương pháp thay thế bao gồm phát triển dựa trên kiểm thử (TDD), nơi mã được tái cấu trúc liên tục như một phần không thể tách rời của quá trình phát triển, trái ngược với một giai đoạn tái cấu trúc riêng biệt.

## Xem thêm
- "Programming in Lua" của Roberto Ierusalimschy về các phương pháp hay nhất và ví dụ.
- "Refactoring: Improving the Design of Existing Code" của Martin Fowler về nguyên tắc áp dụng cho các ngôn ngữ khác nhau.
- Thư mục LuaRocks (https://luarocks.org/) cho các công cụ và mô-đun nhằm bảo trì và tái cấu trúc mã Lua.
