---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:06:35.154390-07:00
description: "T\xE1i c\u1EA5u tr\xFAc l\xE0 qu\xE1 tr\xECnh c\u1EA3i t\u1ED5 m\xE3\
  \ m\xE1y t\xEDnh hi\u1EC7n c\xF3 m\xE0 kh\xF4ng thay \u0111\u1ED5i h\xE0nh vi b\xEA\
  n ngo\xE0i c\u1EE7a n\xF3. L\u1EADp tr\xECnh vi\xEAn t\xE1i c\u1EA5u tr\xFAc \u0111\
  \u1EC3 c\u1EA3i thi\u1EC7n c\xE1c thu\u1ED9c t\xEDnh\u2026"
lastmod: '2024-03-13T22:44:37.352423-06:00'
model: gpt-4-0125-preview
summary: "T\xE1i c\u1EA5u tr\xFAc l\xE0 qu\xE1 tr\xECnh c\u1EA3i t\u1ED5 m\xE3 m\xE1\
  y t\xEDnh hi\u1EC7n c\xF3 m\xE0 kh\xF4ng thay \u0111\u1ED5i h\xE0nh vi b\xEAn ngo\xE0\
  i c\u1EE7a n\xF3. L\u1EADp tr\xECnh vi\xEAn t\xE1i c\u1EA5u tr\xFAc \u0111\u1EC3\
  \ c\u1EA3i thi\u1EC7n c\xE1c thu\u1ED9c t\xEDnh\u2026"
title: "T\xE1i c\u1EA5u tr\xFAc m\xE3"
weight: 19
---

## Là gì & Tại sao?

Tái cấu trúc là quá trình cải tổ mã máy tính hiện có mà không thay đổi hành vi bên ngoài của nó. Lập trình viên tái cấu trúc để cải thiện các thuộc tính phi chức năng của phần mềm, như khả năng đọc, giảm độ phức tạp, cải thiện khả năng bảo trì, hoặc tăng cường hiệu suất.

## Làm thế nào:

Chúng ta cùng xem qua một ví dụ về việc tái cấu trúc một phương thức Ruby tính tổng các bình phương.

**Trước khi tái cấu trúc:**
```ruby
def sum_of_squares(numbers)
  sum = 0
  numbers.each do |number|
    square = number * number
    sum += square
  end
  sum
end

puts sum_of_squares([1, 2, 3])  # Kết quả: 14
```

**Sau khi tái cấu trúc:**
```ruby
def sum_of_squares(numbers)
  numbers.map { |number| number**2 }.sum
end

puts sum_of_squares([1, 2, 3])  # Kết quả: 14
```

Phiên bản tái cấu trúc sử dụng Enumerable của Ruby để biểu đạt cùng một logic một cách ngắn gọn và rõ ràng hơn. Phương thức `map` biến đổi từng phần tử, và `sum` tổng hợp giá trị của chúng, loại bỏ nhu cầu quản lý vòng lặp và gán biến thủ công.

## Tìm hiểu sâu hơn

Tái cấu trúc có một bối cảnh lịch sử phong phú, bắt nguồn từ các thực hành đầu tiên trong phát triển phần mềm. Các tài liệu đầu tiên có thể được truy cứu về những năm 1990, với những đóng góp quan trọng được thực hiện bởi Martin Fowler trong cuốn sách của ông "Tái Cấu Trúc: Cải Thiện Thiết Kế của Mã Nguyên Thủy", nơi ông cung cấp một danh mục các mẫu tái cấu trúc. Kể từ đó, tái cấu trúc đã trở thành một trụ cột của các thực hành phát triển linh hoạt.

Khi chúng ta nói về các phương án thay thế cho việc tái cấu trúc, chúng ta phải xem xét một cách tiếp cận khác như 'Viết lại', nơi bạn thay thế hệ thống cũ từng phần hoặc toàn bộ hoặc áp dụng các thực hành như 'Đánh giá mã' và 'Lập trình cặp' để cải thiện chất lượng mã một cách dần dần. Tuy nhiên, những điều này không phải là thay thế cho việc tái cấu trúc; chúng bổ sung cho quá trình này.

Về mặt thực hiện, Ruby cung cấp một cú pháp biểu cảm xuất sắc thường dẫn đến mã ngắn gọn, dễ đọc hơn sau khi tái cấu trúc. Các nguyên tắc chính bao gồm DRY (Don't Repeat Yourself - Không lặp lại bản thân), sử dụng tên có ý nghĩa, giữ cho các phương thức ngắn gọn và tập trung vào một nhiệm vụ duy nhất, và sử dụng hiệu quả mô-đun Enumerable của Ruby, như đã thấy trong ví dụ trên. Các công cụ tự động như RuboCop cũng có thể giúp lập trình viên xác định các điểm trong mã có thể được tái cấu trúc.

## Xem thêm

Để tìm hiểu sâu hơn về tái cấu trúc trong Ruby, hãy xem qua các tài nguyên sau:

- Cuốn sách tiên phong của Martin Fowler: [Tái Cấu Trúc: Cải Thiện Thiết Kế của Mã Nguyên Thủy](https://martinfowler.com/books/refactoring.html)
- Hướng dẫn về phong cách Ruby để viết mã sạch hơn: [Hướng Dẫn Phong Cách Ruby](https://rubystyle.guide/)
- RuboCop, một phân tích cú pháp mã tĩnh (lint) và bộ định dạng: [Kho Lưu Trữ RuboCop trên GitHub](https://github.com/rubocop/rubocop)
