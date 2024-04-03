---
changelog:
- 2024-01-30, gpt-4-0125-preview, translated from English
date: 2024-01-30 19:11:11.188314-07:00
description: "L\xE0m th\u1EBF n\xE0o: Fish kh\xF4ng h\u1ED7 tr\u1EE3 m\u1EA3ng k\u1EBF\
  t h\u1EE3p natively nh\u01B0 Bash 4+, nh\u01B0ng b\u1EA1n c\xF3 th\u1EC3 \u0111\u1EA1\
  t \u0111\u01B0\u1EE3c ch\u1EE9c n\u0103ng t\u01B0\u01A1ng t\u1EF1 b\u1EB1ng c\xE1\
  ch s\u1EED d\u1EE5ng k\u1EBFt h\u1EE3p danh s\xE1ch v\xE0 thao\u2026"
lastmod: '2024-03-13T22:44:37.198528-06:00'
model: gpt-4-0125-preview
summary: "Fish kh\xF4ng h\u1ED7 tr\u1EE3 m\u1EA3ng k\u1EBFt h\u1EE3p natively nh\u01B0\
  \ Bash 4+, nh\u01B0ng b\u1EA1n c\xF3 th\u1EC3 \u0111\u1EA1t \u0111\u01B0\u1EE3c\
  \ ch\u1EE9c n\u0103ng t\u01B0\u01A1ng t\u1EF1 b\u1EB1ng c\xE1ch s\u1EED d\u1EE5\
  ng k\u1EBFt h\u1EE3p danh s\xE1ch v\xE0 thao t\xE1c chu\u1ED7i."
title: "S\u1EED d\u1EE5ng m\u1EA3ng li\xEAn k\u1EBFt"
weight: 15
---

## Làm thế nào:
Fish không hỗ trợ mảng kết hợp natively như Bash 4+, nhưng bạn có thể đạt được chức năng tương tự bằng cách sử dụng kết hợp danh sách và thao tác chuỗi. Dưới đây là cách mô phỏng chúng:

Trước tiên, thiết lập từng phần tử "mảng kết hợp" riêng lẻ:

```Fish Shell
set food_color_apple "red"
set food_color_banana "yellow"
```

Để truy cập một phần tử, chỉ cần tham chiếu trực tiếp:

```Fish Shell
echo $food_color_apple
# Kết quả: red
```

Nếu bạn cần lặp qua chúng, sử dụng vòng lặp for cùng với quy ước đặt tên:

```Fish Shell
for food in apple banana
    echo $food_color_$food
end
# Kết quả:
# red
# yellow
```

Đối với những người thiếu Bash's `${!array[@]}` để lấy tất cả khóa, bạn có thể lưu khóa trong một danh sách riêng biệt:

```Fish Shell
set food_keys apple banana

for key in $food_keys
    echo $key 'là' $food_color_$key
end
# Kết quả:
# apple là red
# banana là yellow
```

## Sâu hơn
Mảng kết hợp thực sự như trong các ngôn ngữ kịch bản khác chưa phải là một phần của cách tiếp cận của Fish. Giải pháp được hiển thị tận dụng khả năng thao tác chuỗi và danh sách của Fish để tạo ra một cấu trúc mảng kết hợp giả mạo. Mặc dù nó hoạt động, nhưng không sạch sẽ hoặc chống lỗi như sẽ có được nếu hỗ trợ mảng kết hợp được tích hợp sẵn. Các shell khác như Bash và Zsh cung cấp chức năng mảng kết hợp được tích hợp sẵn, dẫn đến mã dễ đọc, dễ hiểu hơn. Tuy nhiên, triết lý thiết kế của Fish hướng đến sự đơn giản và thân thiện với người dùng, có thể phải chịu thiệt hại bởi những tính năng đó. Giải pháp này đáp ứng hầu hết nhu cầu nhưng hãy chú ý đến sự phát triển của Fish Shell - nhóm phát triển của nó liên tục cải thiện và thêm tính năng dựa trên phản hồi từ cộng đồng.
