---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:07:21.654332-07:00
description: "X\xF3a d\u1EA5u ngo\u1EB7c kh\u1ECFi m\u1ED9t chu\u1ED7i c\xF3 ngh\u0129\
  a l\xE0 lo\u1EA1i b\u1ECF c\xE1c k\xFD t\u1EF1 d\u1EA5u ngo\u1EB7c \u0111\u01A1\
  n (`'`) ho\u1EB7c d\u1EA5u ngo\u1EB7c k\xE9p (`\"`) bao quanh \u0111\u1ECBnh ngh\u0129\
  a chu\u1ED7i trong code. L\u1EADp tr\xECnh\u2026"
lastmod: '2024-03-13T22:44:36.302855-06:00'
model: gpt-4-0125-preview
summary: "X\xF3a d\u1EA5u ngo\u1EB7c kh\u1ECFi m\u1ED9t chu\u1ED7i c\xF3 ngh\u0129\
  a l\xE0 lo\u1EA1i b\u1ECF c\xE1c k\xFD t\u1EF1 d\u1EA5u ngo\u1EB7c \u0111\u01A1\
  n (`'`) ho\u1EB7c d\u1EA5u ngo\u1EB7c k\xE9p (`\"`) bao quanh \u0111\u1ECBnh ngh\u0129\
  a chu\u1ED7i trong code. L\u1EADp tr\xECnh\u2026"
title: "Lo\u1EA1i b\u1ECF d\u1EA5u ngo\u1EB7c k\xE9p kh\u1ECFi chu\u1ED7i"
weight: 9
---

## Cái gì & Tại sao?
Xóa dấu ngoặc khỏi một chuỗi có nghĩa là loại bỏ các ký tự dấu ngoặc đơn (`'`) hoặc dấu ngoặc kép (`"`) bao quanh định nghĩa chuỗi trong code. Lập trình viên làm điều này vì nhiều lý do, như định dạng đầu ra, làm sạch đầu vào người dùng, hoặc chuẩn bị chuỗi cho việc phân tích cú pháp hoặc lưu trữ khi mà dấu ngoặc là không cần thiết hoặc có thể gây ra lỗi.

## Làm thế nào:
Đây là hướng dẫn không lan man về cách cắt bỏ những dấu ngoặc phiền phức khỏi chuỗi của bạn trong TypeScript.

```typescript
// Lựa chọn A: Thay thế dấu ngoặc đơn hoặc dấu ngoặc kép bằng regex
function removeQuotes(input: string): string {
  return input.replace(/^["']|["']$/g, '');
}

console.log(removeQuotes(`"Quoted string"`)); // Quoted string
console.log(removeQuotes(`'Another one'`)); // Another one

// Lựa chọn B: Xử lý chuỗi bắt đầu và kết thúc bằng dấu ngoặc khác nhau
function removeMismatchedQuotes(input: string): string {
  return input.replace(/^(['"])(.*?)(?<!\1)\1$/, '$2');
}

console.log(removeMismatchedQuotes(`"Mismatched'`)); // "Mismatched'

// Lựa chọn C: Cắt các loại dấu ngoặc đa dạng
function removeAllQuotes(input: string): string {
  return input.replace(/['"]+/g, '');
}

console.log(removeAllQuotes(`"'Mix'n'Match'"`)); // Mix'n'Match
```

## Sâu hơn nữa
Quay lại trước khi TypeScript được biết đến, những người lập trình JavaScript đã phải đối diện với những chuyện rắc rối về dấu ngoặc, và câu chuyện cũng gần như không thay đổi với TypeScript. Theo thời gian, cách chúng ta cắt chuỗi cũng thay đổi. Ngày nay, với sức mạnh của regex, chúng ta đẩy lùi việc sử dụng cắt chuỗi cồng kềnh hoặc các phương pháp khác khó khăn.

Mặc dù các ví dụ trên nên đáp ứng hầu hết nhu cầu của bạn, nhớ rằng, việc sử dụng dấu ngoặc có thể trở nên phức tạp. Dấu ngoặc lồng nhau, không khớp, và thoát ra là những kẻ lừa đảo chờ đợi để làm bạn vấp ngã. Đối với những trường hợp này, bạn có thể cần các mẫu phức tạp hơn hoặc thậm chí là các trình phân tích cú pháp để xử lý mọi trường hợp khó khăn.

Các phương án khác? Một số người thích sử dụng thư viện như lodash, với các phương thức như `trim` và `trimStart` / `trimEnd`, có thể được tùy chỉnh để cắt dấu ngoặc nếu bạn thiết lập các ký tự bạn muốn cắt bỏ.

Và đối với các bạn hâm mộ TypeScript, đừng quên về các kiểu dữ liệu. Trong khi ở đây chúng ta chủ yếu đang xử lý với chuỗi, khi bạn làm việc với đầu vào người dùng hoặc phân tích cú pháp, việc thêm vào một số bảo vệ kiểu dữ liệu hoặc thậm chí là tổng quát có thể giúp đảm bảo bạn giữ cho mã của bạn an toàn như việc dấu ngoặc của bạn được cắt gọn.

## Xem thêm
Kiểm tra những nơi trực tuyến này để biết thêm thông tin:

- MDN Web Docs về regex (https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Regular_Expressions)
- Tài liệu chính thức của TypeScript (https://www.typescriptlang.org/docs/)
- Bạn Không Cần Lodash/Underscore – Trợ giúp Chuỗi (https://github.com/you-dont-need/You-Dont-Need-Lodash-Underscore#strings)
- Stack Overflow: Đi qua những nơi mà vô số nhà phát triển đã chiến đấu với thảm họa dấu ngoặc (https://stackoverflow.com/)
