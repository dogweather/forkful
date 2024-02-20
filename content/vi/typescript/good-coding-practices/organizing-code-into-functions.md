---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:03:39.692179-07:00
description: "T\u1ED5 ch\u1EE9c code th\xE0nh c\xE1c h\xE0m c\xF3 ngh\u0129a l\xE0\
  \ chia nh\u1ECF code c\u1EE7a b\u1EA1n th\xE0nh c\xE1c kh\u1ED1i c\xF3 th\u1EC3\
  \ t\xE1i s\u1EED d\u1EE5ng, linh ho\u1EA1t. Ch\xFAng ta l\xE0m \u0111i\u1EC1u n\xE0\
  y \u0111\u1EC3 gi\u1EEF cho m\u1ECDi th\u1EE9 kh\xF4ng\u2026"
lastmod: 2024-02-19 22:04:55.476796
model: gpt-4-0125-preview
summary: "T\u1ED5 ch\u1EE9c code th\xE0nh c\xE1c h\xE0m c\xF3 ngh\u0129a l\xE0 chia\
  \ nh\u1ECF code c\u1EE7a b\u1EA1n th\xE0nh c\xE1c kh\u1ED1i c\xF3 th\u1EC3 t\xE1\
  i s\u1EED d\u1EE5ng, linh ho\u1EA1t. Ch\xFAng ta l\xE0m \u0111i\u1EC1u n\xE0y \u0111\
  \u1EC3 gi\u1EEF cho m\u1ECDi th\u1EE9 kh\xF4ng\u2026"
title: "S\u1EAFp x\u1EBFp m\xE3 th\xE0nh c\xE1c h\xE0m"
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
