---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:59:55.919454-07:00
description: "R\xFAt tr\xEDch chu\u1ED7i con c\xF3 ngh\u0129a l\xE0 l\u1EA5y ra c\xE1\
  c ph\u1EA7n c\u1EE5 th\u1EC3 c\u1EE7a m\u1ED9t chu\u1ED7i. L\u1EADp tr\xECnh vi\xEA\
  n th\u1EF1c hi\u1EC7n \u0111i\u1EC1u n\xE0y \u0111\u1EC3 c\xF4 l\u1EADp d\u1EEF\
  \ li\u1EC7u, l\xE0m s\u1EA1ch \u0111\u1EA7u v\xE0o, ho\u1EB7c ph\xE2n\u2026"
lastmod: '2024-02-25T18:49:35.541220-07:00'
model: gpt-4-0125-preview
summary: "R\xFAt tr\xEDch chu\u1ED7i con c\xF3 ngh\u0129a l\xE0 l\u1EA5y ra c\xE1\
  c ph\u1EA7n c\u1EE5 th\u1EC3 c\u1EE7a m\u1ED9t chu\u1ED7i. L\u1EADp tr\xECnh vi\xEA\
  n th\u1EF1c hi\u1EC7n \u0111i\u1EC1u n\xE0y \u0111\u1EC3 c\xF4 l\u1EADp d\u1EEF\
  \ li\u1EC7u, l\xE0m s\u1EA1ch \u0111\u1EA7u v\xE0o, ho\u1EB7c ph\xE2n\u2026"
title: "Tr\xEDch xu\u1EA5t chu\u1ED7i con"
---

{{< edit_this_page >}}

## Cái gì & Tại sao?
Rút trích chuỗi con có nghĩa là lấy ra các phần cụ thể của một chuỗi. Lập trình viên thực hiện điều này để cô lập dữ liệu, làm sạch đầu vào, hoặc phân tích thông tin cho quá trình xử lý tiếp theo.

## Cách thực hiện:
Trong Fish, bạn sử dụng lệnh `string` để thao tác với chuỗi. Dưới đây là cách làm:

### Lấy từ đầu:
```Fish Shell
set my_string "Fish Shell is fun!"
echo $my_string | string sub -l 4 # Xuất ra 'Fish'
```

### Cắt từ cuối:
```Fish Shell
set my_string "Fish Shell is fun!"
echo $my_string | string sub -s -4 # Xuất ra 'fun!'
```

### Phạm vi cụ thể:
```Fish Shell
set my_string "Fish Shell is fun!"
echo $my_string | string sub -s 6 -l 5 # Xuất ra 'Shell'
```

## Sâu hơn nữa
Ngày xưa, chúng ta thường cắt và chia các chuỗi trong Fish bằng cách sử dụng các công cụ bên ngoài như `cut`, `awk`, hoặc `sed`. Bây giờ, `string` là hàm tích hợp sẵn mà chúng ta thường xuyên sử dụng, được giới thiệu trong Fish 2.3.0. Nó nhanh hơn, dễ đọc hơn, và tích hợp mượt mà với các kịch bản của chúng ta.

`string sub` không phải là lựa chọn duy nhất của bạn. Các hàm `string` khác có thể chia chuỗi, thay thế các phần, hoặc kết nối chúng. Tập trung này vào việc sử dụng tối thiểu tài nguyên và dễ hiểu.

Về việc triển khai, khi bạn rút trích chuỗi con, Fish đọc chuỗi và chỉ xuất ra phần bạn đã chỉ định, đồng thời tôn trọng mã hóa ký tự và tránh các lỗi thông thường trong rút trích chuỗi con, như chia đôi một ký tự.

## Xem thêm
- Tài liệu chính thức của Fish về `string`: https://fishshell.com/docs/current/cmds/string.html
- Hướng dẫn cộng đồng về lập kịch bản Fish: https://fishshell.com/docs/current/tutorial.html
- Thảo luận trên Stack Overflow về thao tác chuỗi Fish: https://stackoverflow.com/questions/tagged/fish
