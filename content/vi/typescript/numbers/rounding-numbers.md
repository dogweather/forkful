---
title:                "Làm tròn số"
aliases: - /vi/typescript/rounding-numbers.md
date:                  2024-01-28T22:07:10.408558-07:00
model:                 gpt-4-0125-preview
simple_title:         "Làm tròn số"

tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/typescript/rounding-numbers.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cái gì & Tại sao?
Làm tròn số là việc cắt bớt một số để có độ chính xác nhất định. Lập trình viên làm điều này để kiểm soát đầu ra số học cho dễ đọc, mục đích hiển thị, hoặc khi cần độ chính xác nhất định sau các phép toán cho kết quả dạng số dấu phẩy động.

## Làm thế nào:
Việc làm tròn trong TypeScript có thể được thực hiện bằng nhiều phương pháp. Dưới đây là một số cách nhanh chóng:

```typescript
// Math.round làm tròn đến số nguyên gần nhất
console.log(Math.round(1.5)); // Kết quả: 2

// Math.ceil làm tròn lên đến số nguyên gần nhất
console.log(Math.ceil(1.1)); // Kết quả: 2

// Math.floor làm tròn xuống đến số nguyên gần nhất
console.log(Math.floor(1.8)); // Kết quả: 1

// toFixed làm tròn đến một số lượng chữ số thập phân cố định
let num = 1.23456;
console.log(num.toFixed(2)); // Kết quả: "1.23"
// Lưu ý: toFixed trả về một chuỗi! Sử dụng parseFloat để chuyển đổi lại nếu cần.
console.log(parseFloat(num.toFixed(2))); // Kết quả: 1.23
```

## Sâu hơn
Ngày xưa, việc làm tròn là một yêu cầu bắt buộc do không gian lưu trữ hạn chế và vấn đề về độ chính xác trong các máy tính đời đầu. Ngày nay, tính toán số dấu phẩy động có thể dẫn đến kết quả kỳ lạ do cách số được lưu trữ dưới dạng nhị phân. Các phương pháp thay thế cho việc làm tròn bao gồm floor, ceil, và trunc (để cắt bỏ phần thập phân mà không làm tròn).

Điều đáng chú ý bên trong: `Math.round` tuân theo "làm tròn nửa lên" (còn gọi là "làm tròn thương mại"), trong khi `Math.floor` và `Math.ceil` là đơn giản. `toFixed` có thể gây ra kết quả không mong đợi vì nó trả về một chuỗi, và nó làm tròn theo "làm tròn nửa đến chẵn" (còn gọi là "làm tròn ngân hàng"), đặc biệt hữu ích để giảm thiên vị khi làm tròn cùng một số nhiều lần.

## Tham khảo thêm
- [MDN - Math.round()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math/round)
- [MDN - Math.ceil()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math/ceil)
- [MDN - Math.floor()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math/floor)
- [MDN - toFixed()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Number/toFixed)
- [Tiêu chuẩn IEEE cho Số học Điểm Dấu Phẩy Động (IEEE 754)](https://ieeexplore.ieee.org/document/4610935)
