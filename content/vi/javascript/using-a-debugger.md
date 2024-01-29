---
title:                "Sử dụng bộ gỡ lỗi"
date:                  2024-01-28T22:09:20.703807-07:00
model:                 gpt-4-0125-preview
simple_title:         "Sử dụng bộ gỡ lỗi"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/javascript/using-a-debugger.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cái gì & Tại sao?
Sử dụng debugger có nghĩa là tận dụng các công cụ chuyên biệt giúp bạn có cái nhìn sâu vào bên trong mã của mình, theo dõi nó chạy từng bước một. Lập trình viên làm điều này để tìm và xóa lỗi, tối ưu hóa hiệu suất và hiểu rõ hơn về hành vi của mã.

## Cách thực hiện:
Dưới đây là một đoạn mã JavaScript không hoạt động như mong đợi:

```javascript
function buggyMultiply(a, b) {
    return a + b; // Ôi! Đây phải là phép nhân, không phải phép cộng.
}

let result = buggyMultiply(5, 3);
console.log('Kết quả:', result);
```

Kết quả không chính xác:
```
Kết quả: 8
```

Hãy debug trong Chrome DevTools:

1. Mở JS này trong trình duyệt.
2. Click chuột phải và chọn "Inspect" để mở DevTools.
3. Click vào tab "Sources".
4. Tìm đoạn mã hoặc trang của bạn và đặt một điểm dừng (breakpoint) bằng cách click vào số dòng bên cạnh câu lệnh `return`.
5. Làm mới trang để kích hoạt điểm dừng.
6. Kiểm tra bảng "Scope" để xem các biến cục bộ `a` và `b`.
7. Bước qua với nút "Step over next function call".
8. Nhận diện lỗi trong câu lệnh `return`.
9. Sửa đoạn mã:
```javascript
function buggyMultiply(a, b) {
    return a * b; // Đã sửa!
}

let result = buggyMultiply(5, 3);
console.log('Kết quả:', result);
```

Kết quả đã sửa:
```
Kết quả: 15
```

## Tìm hiểu sâu hơn
Khái niệm về việc debug tồn tại từ những ngày đầu của ngành máy tính - truyền thuyết kể rằng nó bắt đầu khi một con bướm được tìm thấy trong máy tính vào những năm 1940! Ngày nay, các công cụ debugger JavaScript như công cụ tích hợp sẵn trong trình duyệt (Chrome DevTools, Firefox Developer Tools) hoặc debugger tích hợp trong IDE (Visual Studio Code, WebStorm) cung cấp rất nhiều tính năng.

Những lựa chọn thay thế cho các debugger tích hợp sẵn bao gồm các công cụ của bên thứ ba như WebStorm hoặc sử dụng cách cũ `console.log` để xuất trạng thái của biến. Nhưng những phương pháp này không cung cấp sự tương tác thời gian thực và việc kiểm tra chi tiết như debugger.

Về chi tiết triển khai, hầu hết các debugger hoạt động tương tự nhau: chúng cho phép bạn đặt các điểm dừng, tạm dừng việc thực thi, bước qua mã, kiểm tra trạng thái biến hiện tại, theo dõi các biểu thức và thậm chí là thao tác giá trị ngay lập tức để kiểm thử các tình huống khác nhau.

## Xem thêm
- [Google Chrome DevTools](https://developers.google.com/web/tools/chrome-devtools)
- [Mạng lưới Nhà phát triển Mozilla - Firefox Debugger](https://developer.mozilla.org/en-US/docs/Tools/Debugger)
- [Visual Studio Code - Debugging](https://code.visualstudio.com/docs/editor/debugging)
