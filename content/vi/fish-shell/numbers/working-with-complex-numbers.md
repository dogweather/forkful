---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:12:21.384248-07:00
description: "S\u1ED1 ph\u1EE9c m\u1EDF r\u1ED9ng \xFD t\u01B0\u1EDFng v\u1EC1 \u0111\
  \u01B0\u1EDDng s\u1ED1 m\u1ED9t chi\u1EC1u th\xE0nh m\u1ED9t m\u1EB7t ph\u1EB3ng\
  \ ph\u1EE9c t\u1EA1p hai chi\u1EC1u. L\u1EADp tr\xECnh vi\xEAn s\u1EED d\u1EE5ng\
  \ ch\xFAng trong c\xE1c l\u0129nh v\u1EF1c nh\u01B0 k\u1EF9 thu\u1EADt, v\u1EAD\
  t l\xFD\u2026"
lastmod: '2024-03-13T22:44:37.199843-06:00'
model: gpt-4-0125-preview
summary: "S\u1ED1 ph\u1EE9c m\u1EDF r\u1ED9ng \xFD t\u01B0\u1EDFng v\u1EC1 \u0111\u01B0\
  \u1EDDng s\u1ED1 m\u1ED9t chi\u1EC1u th\xE0nh m\u1ED9t m\u1EB7t ph\u1EB3ng ph\u1EE9\
  c t\u1EA1p hai chi\u1EC1u."
title: "L\xE0m vi\u1EC7c v\u1EDBi s\u1ED1 ph\u1EE9c"
weight: 14
---

## Làm thế nào:
Trong Fish, chúng ta xử lý số phức bằng cách sử dụng `math` với phần thực và phần ảo. Dưới đây là một khởi đầu:

```fish
# Cộng hai số phức (3+4i) và (5+2i)
set complex_sum (math "3+4i + 5+2i")
echo $complex_sum # Kết quả: 8+6i

# Nhân hai số phức (1+2i) và (3+4i)
set complex_prod (math "1+2i * 3+4i")
echo $complex_prod # Kết quả: -5+10i
```

Nếu bạn cần nâng một số phức lên một lũy thừa hoặc lấy dạng mũ của nó:

```fish
# Bình phương của (2+3i)
set complex_square (math "(2+3i)^2")
echo $complex_square # Kết quả: -5+12i

# Mũ của (2i)
set complex_exp (math "e^(2i)")
echo $complex_exp # Kết quả: -0.41615+0.9093i
```

## Sâu hơn
Hỗ trợ số phức trong Fish Shell tương đối mới, bắt đầu từ phiên bản 3.1.0. Trước đó, mọi người có thể đã sử dụng `bc` hoặc gọi ra các công cụ bên ngoài như Python để tính toán phức tạp.

Các phương án thay thế cho math của Fish bao gồm các thư viện số học chuyên biệt hoặc ngôn ngữ như MATLAB, Python với NumPy, hoặc thậm chí C++ với Thư viện Chuẩn. Tuy nhiên, những cái này có thể quá mức cần thiết cho các tính toán nhanh trên shell.

Hỗ trợ số phức của Fish được tích hợp sẵn trong lệnh `math` nội bộ của nó, tận dụng libcalc. Điều này có nghĩa là bạn không cần phải cài đặt các công cụ bổ sung cho các thao tác cơ bản.

Tuy nhiên, Fish không được thiết kế cho tính toán toán học nặng. Khả năng toán học của nó thuận tiện cho các tính toán nhanh hoặc kịch bản khi số phức xuất hiện, nhưng cân nhắc sử dụng các công cụ mạnh mẽ hơn cho các nhiệm vụ đòi hỏi tính toán nặng nề.

## Xem thêm
- Tài liệu về Fish shell cho math: https://fishshell.com/docs/current/commands.html#math
- NumPy cho Python, một lựa chọn phổ biến: https://numpy.org/
- Một cái nhìn sâu sắc hơn vào số phức: https://betterexplained.com/articles/a-visual-intuitive-guide-to-imaginary-numbers/
