---
title:                "Làm việc với số phức"
date:                  2024-01-28T22:12:21.384248-07:00
model:                 gpt-4-0125-preview
simple_title:         "Làm việc với số phức"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/fish-shell/working-with-complex-numbers.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cái gì & Tại sao?
Số phức mở rộng ý tưởng về đường số một chiều thành một mặt phẳng phức tạp hai chiều. Lập trình viên sử dụng chúng trong các lĩnh vực như kỹ thuật, vật lý và đồ họa cho các tính toán yêu cầu hai thành phần, như tín hiệu hoặc xoay vòng.

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
