---
title:                "Loại bỏ dấu ngoặc kép khỏi chuỗi"
aliases:
- /vi/javascript/removing-quotes-from-a-string.md
date:                  2024-01-28T22:06:58.291421-07:00
model:                 gpt-4-0125-preview
simple_title:         "Loại bỏ dấu ngoặc kép khỏi chuỗi"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/javascript/removing-quotes-from-a-string.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cái gì & Tại sao?
Loại bỏ dấu ngoặc kép khỏi một chuỗi có nghĩa là bỏ đi những dấu ngoặc kép phiền phức có thể làm rối tung code của bạn, đặc biệt khi bạn đang phân tích dữ liệu hoặc xây dựng các đối tượng JSON. Các lập trình viên thực hiện điều này để làm sạch đầu vào, tránh lỗi cú pháp, và khiến cho chuỗi hợp tác tốt với các phần khác của code.

## Cách thức:
Hãy tưởng tượng bạn có một chuỗi được bao bởi dấu ngoặc kép, như `"\"Hello, World!\""` và bạn muốn văn bản nguyên bản, không dấu ngoặc. Dưới đây là một đoạn mã JavaScript nhanh để giải phóng chuỗi của bạn khỏi những "xiềng xích" dấu ngoặc đó:

```javascript
let quotedString = "\"Hello, World!\"";
let unquotedString = quotedString.replace(/^"|"$/g, '');
console.log(unquotedString); // Kết quả: Hello, World!
```

Và nếu bạn đang xử lý dấu ngoặc đơn? Chỉ cần điều chỉnh biểu thức chính quy một chút:

```javascript
let singleQuotedString = "'Hello, World!'";
let unquotedString = singleQuotedString.replace(/^'|'$/g, '');
console.log(unquotedString); // Kết quả: Hello, World!
```

Hoặc nếu chuỗi của bạn là sự kết hợp của cả hai? Không vấn đề:

```javascript
let mixedQuotedString = "\"'Hello, World!'\"";
let unquotedString = mixedQuotedString.replace(/^["']|["']$/g, '');
console.log(unquotedString); // Kết quả: 'Hello, World!'
```

## Sâu hơn nữa
Trước khi JSON trở nên phổ biến, việc thoát khỏi dấu ngoặc là một cao nguyên hoang dã của dấu gạch chéo ngược và những thủ thuật đặc biệt. Các ngôn ngữ lập trình sớm không luôn luôn hợp tác tốt với dấu ngoặc, điều này có nghĩa là rất nhiều thao tác chuỗi thủ công. Bây giờ, với các định dạng dữ liệu được chuẩn hóa, việc loại bỏ dấu ngoặc thường là về việc làm sạch đầu vào trước khi chúng được xử lý như JSON hoặc lưu trữ văn bản mà không có xung đột định dạng.

Các phương án thay thế cho `.replace()`? Chắc chắn! Bạn có thể tách và kết hợp một chuỗi dựa trên dấu ngoặc, sử dụng slice nếu bạn chắc chắn về vị trí của những dấu ngoặc, hoặc thậm chí là sử dụng biểu thức chính quy để kéo ra văn bản cần thiết. Tất cả phụ thuộc vào bối cảnh.

Nhưng đừng quên về các trường hợp ngoại lệ: dấu ngoặc trong dấu ngoặc, dấu ngoặc được thoát, và kí tự quốc tế. Hãy coi chuỗi của bạn như một mảnh đất của các ngoại lệ tiềm ẩn, và tiến hành cẩn thận. Các trình thực thi JavaScript hiện đại được tối ưu hóa để xử lý các thao tác biểu thức chính quy một cách hiệu quả, vì vậy chúng thường là phương án đi đến, nhưng luôn đáng để kiểm tra hiệu suất cho các nhiệm vụ xử lý dữ liệu nặng.

## Xem thêm
Đào sâu hơn vào việc thao tác chuỗi và biểu thức chính quy:

- Mạng Lưới Nhà Phát Triển Mozilla về String.replace(): https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/replace
- Regex101 để thử nghiệm các mẫu biểu thức chính quy của bạn: https://regex101.com/
- JSON.org để hiểu tại sao chúng ta phải xử lý nhiều dấu ngoặc trong phát triển web hiện đại: http://json.org/
