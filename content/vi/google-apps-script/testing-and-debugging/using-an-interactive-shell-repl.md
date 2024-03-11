---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:04:24.168361-07:00
description: "Giao di\u1EC7n l\u1EADp tr\xECnh t\u01B0\u01A1ng t\xE1c, hay v\xF2ng\
  \ l\u1EB7p \u0110\u1ECDc-\u0110\xE1nh gi\xE1-In (REPL), l\xE0 m\u1ED9t m\xF4i tr\u01B0\
  \u1EDDng l\u1EADp tr\xECnh t\u01B0\u01A1ng t\xE1c \u0111\u01A1n gi\u1EA3n, l\u1EA5\
  y \u0111\u1EA7u v\xE0o \u0111\u01A1n l\u1EBB t\u1EEB ng\u01B0\u1EDDi d\xF9ng (bi\u1EC3\
  u\u2026"
lastmod: '2024-03-11T00:14:09.269378-06:00'
model: gpt-4-0125-preview
summary: "Giao di\u1EC7n l\u1EADp tr\xECnh t\u01B0\u01A1ng t\xE1c, hay v\xF2ng l\u1EB7\
  p \u0110\u1ECDc-\u0110\xE1nh gi\xE1-In (REPL), l\xE0 m\u1ED9t m\xF4i tr\u01B0\u1EDD\
  ng l\u1EADp tr\xECnh t\u01B0\u01A1ng t\xE1c \u0111\u01A1n gi\u1EA3n, l\u1EA5y \u0111\
  \u1EA7u v\xE0o \u0111\u01A1n l\u1EBB t\u1EEB ng\u01B0\u1EDDi d\xF9ng (bi\u1EC3u\u2026"
title: "S\u1EED d\u1EE5ng giao di\u1EC7n d\xF2ng l\u1EC7nh t\u01B0\u01A1ng t\xE1c\
  \ (REPL)"
---

{{< edit_this_page >}}

## Là gì & Tại sao?

Giao diện lập trình tương tác, hay vòng lặp Đọc-Đánh giá-In (REPL), là một môi trường lập trình tương tác đơn giản, lấy đầu vào đơn lẻ từ người dùng (biểu thức), đánh giá chúng và trả kết quả lại cho người dùng. Lập trình viên sử dụng REPL cho việc tạo mẫu nhanh, gỡ lỗi và học cú pháp cũng như cách hoạt động của ngôn ngữ lập trình một cách tương tác.

## Làm thế nào:

Google Apps Script, một ngôn ngữ lập trình dựa trên đám mây để tự động hóa các nhiệm vụ qua các sản phẩm của Google, không có công cụ REPL tích hợp giống như các ngôn ngữ khác như Python hay Node.js của JavaScript. Tuy nhiên, bạn có thể mô phỏng trải nghiệm tương tự sử dụng các tính năng ghi nhật ký và gỡ lỗi của Trình soạn thảo Apps Script hoặc bằng cách thiết lập một môi trường bên ngoài. Ở đây, chúng tôi tập trung vào việc tạo một REPL tạm thời trong Trình soạn thảo Apps Script.

1. **Tạo một hàm REPL tạm thời**:

```javascript
function myREPL() {
  var input = Logger.log('Nhập biểu thức của bạn: ');
  try {
    var result = eval(input);
    Logger.log('Kết quả: ' + result);
  } catch(e) {
    Logger.log('Lỗi: ' + e.message);
  }
}
```

Vì đầu vào trực tiếp từ người dùng không khả thi theo cùng một cách như một REPL truyền thống trong môi trường Apps Script, bạn có thể chỉnh sửa biến `input` một cách thủ công và chạy `myREPL()` để thử nghiệm các biểu thức.

2. **Thực thi Mã Mẫu**:

Giả sử bạn muốn đánh giá `2+2`. Bạn sẽ chỉnh sửa hàm `myREPL` như sau:

```javascript
function myREPL() {
  var input = '2+2'; // Nhập biểu thức của bạn ở đây một cách thủ công
  // Phần còn lại giữ nguyên...
}
```

Sau khi chạy `myREPL()`, kiểm tra Nhật ký (Xem > Nhật ký) để xem đầu ra, nó sẽ thể hiện như sau:

```
[20-xx-xxxx xx:xx:xx:xxx] Nhập biểu thức của bạn:
[20-xx-xxxx xx:xx:xx:xxx] Kết quả: 4
```

3. **Gỡ lỗi với Logger**:

Đối với việc gỡ lỗi phức tạp hơn, xen kẽ `Logger.log(biến);` trong mã của bạn để in trạng thái của biến, giúp bạn hiểu dòng chảy và trạng thái trung gian của các script của mình.

## Sâu hơn nữa

Khái niệm về REPL gắn liền sâu rễ trong lịch sử của việc tính toán, bắt nguồn từ thời hệ thống chia sẻ thời gian của những năm 1960, cho phép các phiên tương tác. Ngôn ngữ như Lisp đã phát triển mạnh mẽ trong môi trường này, vì REPL rất quan trọng cho quá trình phát triển lặp lại của chúng. Trái lại, Google Apps Script, xuất hiện sau nhiều, chủ yếu được thiết kế cho web, tập trung vào tự động hóa nhiệm vụ trong bộ sản phẩm của Google hơn là lập trình dựa trên bảng điều khiển lặp lại.

Google Apps Script truyền thống không hỗ trợ phiên mã hóa tương tác, thời gian thực ngay lập tức do bản chất dựa trên đám mây và tập trung vào triển khai ứng dụng web của nó. Mô hình thực thi của nó xoay quanh các hàm được kích hoạt bởi sự kiện web, kích hoạt theo thời gian, hoặc kích hoạt thủ công trong môi trường, thay vì các vòng lặp phản hồi tức thì mà REPL cung cấp.

Mặc dù REPL và trình gỡ lỗi tạm thời trong Trình soạn thảo Apps Script cung cấp một số mức độ tương tác, chúng không hoàn toàn tái tạo lại sự phản hồi tức thì và hiệu quả của các REPL truyền thống được tìm thấy trong nhiều ngôn ngữ lập trình. Các nhà phát triển tìm kiếm trải nghiệm REPL thực sự với công nghệ Google có thể khám phá các môi trường JavaScript bên ngoài hoặc Node.js với các API của Google. Các giải pháp này có thể cung cấp một phiên mã hóa tương tác và phản hồi nhanh hơn, mặc dù đòi hỏi nhiều thiết lập hơn và có khả năng bước ra ngoài môi trường Apps Script trực tiếp.
