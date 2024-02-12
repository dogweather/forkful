---
title:                "Sắp xếp mã thành các hàm"
aliases:
- /vi/typescript/organizing-code-into-functions.md
date:                  2024-01-28T22:03:39.692179-07:00
model:                 gpt-4-0125-preview
simple_title:         "Sắp xếp mã thành các hàm"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/typescript/organizing-code-into-functions.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Lý do và Mục đích
Tổ chức code thành các hàm có nghĩa là chia nhỏ code của bạn thành các khối có thể tái sử dụng, linh hoạt. Chúng ta làm điều này để giữ cho mọi thứ không bị lặp lại (Don't Repeat Yourself - DRY), làm cho code sạch sẽ hơn, dễ đọc hơn và dễ gỡ lỗi hơn.

## Cách thực hiện:
Hãy tưởng tượng bạn đang tạo một máy tính cơ bản. Thay vì viết logic cộng ở mọi nơi bạn cần, hãy tạo một hàm `add`:

```TypeScript
function add(x: number, y: number): number {
  return x + y;
}

console.log(add(5, 7)); // Kết quả mẫu: 12
```

Bây giờ, giả sử chúng ta cần một hàm để nhân:

```TypeScript
function multiply(x: number, y: number): number {
  return x * y;
}

console.log(multiply(3, 4)); // Kết quả mẫu: 12
```
Nhận thấy làm thế nào chúng ta tập trung vào một nhiệm vụ cho mỗi hàm? Đó chính là trọng tâm của việc tổ chức code.

## Sâu hơn nữa
Theo lịch sử, khi các ngôn ngữ lập trình tiến hoá, hàm trở nên quan trọng trong việc cấu trúc code, dựa trên các hàm toán học. Chúng là một phần không thể thiếu trong lập trình thủ tục và tiếp tục tồn tại trong các mô hình lập trình hướng đối tượng và lập trình hàm.

Các lựa chọn khác? Bạn có thể không sử dụng hàm, nhưng đó chỉ là một vé một chiều đến Thị Trấn Mì Ý. Hoặc bạn có thể chuyển sang Lập Trình Hướng Đối Tượng (Object-Oriented Programming - OOP) và gói chức năng vào các phương thức - những gì cơ bản là hàm thuộc về đối tượng.

Về mặt triển khai, TypeScript nhấn mạnh vào kiểu. Định rõ kiểu đầu vào và đầu ra cho các hàm không chỉ là phép lịch sự; nó là yêu cầu bắt buộc cho code TypeScript sạch. Hơn nữa, với TypeScript, bạn nhận được những tính năng thú vị như quá tải, generics, và các tham số tùy chọn để tăng cường chức năng cho hàm của bạn.

## Xem Thêm
Hãy xem những nguồn tài liệu này để nâng cao trò chơi hàm của bạn:

- [Sổ tay TypeScript – Hàm](https://www.typescriptlang.org/docs/handbook/2/functions.html): Kinh thánh của bạn cho các hàm TypeScript.
- [Clean Code JavaScript](https://github.com/ryanmcdermott/clean-code-javascript#functions): Áp dụng nguyên tắc Clean Code cho các hàm JavaScript của bạn.
- [You Don’t Know JS – Phạm vi & Đóng gói](https://github.com/getify/You-Dont-Know-JS): Nắm bắt cách hàm hoạt động với phạm vi và đóng gói trong JavaScript.
