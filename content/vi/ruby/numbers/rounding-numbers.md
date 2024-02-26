---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:07:17.848877-07:00
description: "L\xE0m tr\xF2n s\u1ED1 c\xF3 ngh\u0129a l\xE0 \u0111i\u1EC1u ch\u1EC9\
  nh ch\xFAng v\u1EC1 s\u1ED1 nguy\xEAn g\u1EA7n nh\u1EA5t ho\u1EB7c \u0111\u1EBF\
  n m\u1ED9t m\u1EE9c \u0111\u1ED9 ch\xEDnh x\xE1c \u0111\u01B0\u1EE3c ch\u1EC9 \u0111\
  \u1ECBnh. C\xE1c l\u1EADp tr\xECnh vi\xEAn l\xE0m tr\xF2n s\u1ED1 \u0111\u1EC3 \u0111\
  \u01A1n gi\u1EA3n\u2026"
lastmod: '2024-02-25T18:49:35.666211-07:00'
model: gpt-4-0125-preview
summary: "L\xE0m tr\xF2n s\u1ED1 c\xF3 ngh\u0129a l\xE0 \u0111i\u1EC1u ch\u1EC9nh\
  \ ch\xFAng v\u1EC1 s\u1ED1 nguy\xEAn g\u1EA7n nh\u1EA5t ho\u1EB7c \u0111\u1EBFn\
  \ m\u1ED9t m\u1EE9c \u0111\u1ED9 ch\xEDnh x\xE1c \u0111\u01B0\u1EE3c ch\u1EC9 \u0111\
  \u1ECBnh. C\xE1c l\u1EADp tr\xECnh vi\xEAn l\xE0m tr\xF2n s\u1ED1 \u0111\u1EC3 \u0111\
  \u01A1n gi\u1EA3n\u2026"
title: "L\xE0m tr\xF2n s\u1ED1"
---

{{< edit_this_page >}}

## Làm thế nào & Tại sao?
Làm tròn số có nghĩa là điều chỉnh chúng về số nguyên gần nhất hoặc đến một mức độ chính xác được chỉ định. Các lập trình viên làm tròn số để đơn giản hóa, để phù hợp với kỳ vọng của con người, hoặc để đưa dữ liệu vào các định dạng cụ thể—hãy nghĩ đến các tính toán tài chính, hiển thị đồ họa, hoặc giảm kích thước lưu trữ.

## Cách thực hiện:

```Ruby
# Làm tròn cơ bản
puts 3.14159.round      # => 3
puts 2.6.round          # => 3

# Chỉ định độ chính xác
puts 3.14159.round(2)   # => 3.14
puts 2.675.round(2)     # => 2.68

# Làm tròn xuống
puts 2.9.floor          # => 2

# Làm tròn lên
puts 2.1.ceil           # => 3

# Làm tròn về phía không
puts -2.9.round         # => -3
puts -2.9.truncate      # => -2
```

Kết quả mẫu:
```
3
3
3.14
2.68
2
3
-3
-2
```

## Tìm hiểu sâu
Việc làm tròn số không phải là mới—loài người đã làm điều này từ hàng thế kỷ để làm cho các phép tính dễ dàng hơn hoặc để làm việc trong giới hạn của các công cụ của họ. Trong Ruby, phương thức `round` là linh hoạt, có khả năng làm tròn đến số nguyên gần nhất mặc định hoặc đến một vị trí thập phân được chỉ định.

Một phương thức thay thế cho `round` là `floor` dùng để luôn làm tròn xuống, và `ceil` để luôn làm tròn lên, bất kể giá trị của số. Để chỉ cắt bỏ các chữ số thập phân, bạn có `truncate`.

Về mặt lịch sử, khi nói đến máy tính, việc làm tròn trở nên quan trọng trong việc xử lý số học dấu phẩy động do độ chính xác không hoàn hảo của nó. Ruby, giống như hầu hết các ngôn ngữ, tuân theo tiêu chuẩn IEEE 754 cho số dấu phẩy động, có nghĩa là nó xử lý việc làm tròn theo cách mà hầu hết lập trình viên có thể dự đoán và tin tưởng.

Tuy nhiên, còn nhiều hơn thế nữa—những thứ như làm tròn theo ngân hàng (còn được gọi là làm tròn một nửa đến số chẵn) là các khái niệm mà các nhà phát triển Ruby có thể cần phải triển khai thủ công, vì phương thức `round` không cung cấp sẵn nó.

## Xem thêm
- [Tài liệu Ruby](https://ruby-doc.org/core-3.0.0/Float.html#method-i-round) cho phương thức `round` của Float.
- [Tiêu chuẩn IEEE về Số học Dấu phẩy động (IEEE 754)](https://ieeexplore.ieee.org/document/4610935).
- [Hiểu về Độ chính xác Số Dấu phẩy động](https://floating-point-gui.de/), để có cái nhìn sâu sắc hơn về cách máy tính xử lý số thập phân.
