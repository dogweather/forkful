---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:59:55.919454-07:00
description: "C\xE1ch th\u1EF1c hi\u1EC7n: Trong Fish, b\u1EA1n s\u1EED d\u1EE5ng\
  \ l\u1EC7nh `string` \u0111\u1EC3 thao t\xE1c v\u1EDBi chu\u1ED7i. D\u01B0\u1EDB\
  i \u0111\xE2y l\xE0 c\xE1ch l\xE0m."
lastmod: '2024-03-13T22:44:37.193514-06:00'
model: gpt-4-0125-preview
summary: "Trong Fish, b\u1EA1n s\u1EED d\u1EE5ng l\u1EC7nh `string` \u0111\u1EC3 thao\
  \ t\xE1c v\u1EDBi chu\u1ED7i."
title: "Tr\xEDch xu\u1EA5t chu\u1ED7i con"
weight: 6
---

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
