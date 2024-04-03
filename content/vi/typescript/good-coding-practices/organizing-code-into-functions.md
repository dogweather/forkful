---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:03:39.692179-07:00
description: "C\xE1ch th\u1EF1c hi\u1EC7n: H\xE3y t\u01B0\u1EDFng t\u01B0\u1EE3ng\
  \ b\u1EA1n \u0111ang t\u1EA1o m\u1ED9t m\xE1y t\xEDnh c\u01A1 b\u1EA3n. Thay v\xEC\
  \ vi\u1EBFt logic c\u1ED9ng \u1EDF m\u1ECDi n\u01A1i b\u1EA1n c\u1EA7n, h\xE3y t\u1EA1\
  o m\u1ED9t h\xE0m `add`."
lastmod: '2024-03-13T22:44:36.326276-06:00'
model: gpt-4-0125-preview
summary: "H\xE3y t\u01B0\u1EDFng t\u01B0\u1EE3ng b\u1EA1n \u0111ang t\u1EA1o m\u1ED9\
  t m\xE1y t\xEDnh c\u01A1 b\u1EA3n."
title: "S\u1EAFp x\u1EBFp m\xE3 th\xE0nh c\xE1c h\xE0m"
weight: 18
---

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
