---
title:                "Loại bỏ dấu ngoặc kép khỏi chuỗi"
date:                  2024-01-28T22:06:06.819657-07:00
model:                 gpt-4-0125-preview
simple_title:         "Loại bỏ dấu ngoặc kép khỏi chuỗi"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/gleam/removing-quotes-from-a-string.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Là gì & Tại sao?
Loại bỏ dấu ngoặc kép khỏi một chuỗi nghĩa là gỡ bỏ những lớp phụ – các dấu ngoặc kép – khỏi dữ liệu văn bản của bạn. Các lập trình viên làm điều này để làm sạch đầu vào, chuẩn bị các chuỗi cho việc xử lý, hoặc chỉ để giữ mọi thứ gọn gàng và nhất quán trong ứng dụng của họ. Cuối cùng, điều quan trọng là dữ liệu sạch sẽ, có thể sử dụng.

## Làm thế nào:
Việc loại bỏ dấu ngoặc kép trong Gleam khá đơn giản. Chúng ta có thể sử dụng tính năng khớp mẫu hoặc các hàm chuỗi có sẵn. Dưới đây là một ví dụ nhanh để minh họa:

```gleam
pub fn remove_quotes(text: String) -> String {
  let without_quotes = string.trim(text, "\"")
  without_quotes
}

pub fn main() {
  let text_with_quotes = "\"Xin chào, Thế giới!\""
  let cleaned_text = remove_quotes(text_with_quotes)
  io.println(cleaned_text)
}
```

Kết quả mẫu:
```
Xin chào, Thế giới!
```

## Đào sâu
Trong lịch sử, việc xử lý dấu ngoặc kép trong các chuỗi đã là một nhiệm vụ phổ biến trong xử lý văn bản và ngôn ngữ lập trình kịch bản. Do bản chất của chuỗi thường xuyên là đầu vào của người dùng hoặc được đọc từ các tệp, chúng có thể đi kèm với dấu ngoặc kép cần được loại bỏ vì nhiều lý do, như chèn vào cơ sở dữ liệu hay định dạng.

Trong Gleam, chúng ta sử dụng hàm `string.trim` để cạo bỏ các dấu ngoặc kép. Có những phương án khác! Chúng ta có thể lặp qua chuỗi hoặc áp dụng biểu thức chính quy, nhưng `string.trim` là công cụ tiện lợi cho công việc này bởi vì độ ngắn gọn và hiệu suất của nó.

Nếu chúng ta khám phá chi tiết về cách thức hoạt động, `string.trim` hoạt động bằng cách loại bỏ các ký tự ở đầu và cuối chuỗi khớp với mẫu đã cung cấp. Vì vậy, nếu bạn có dấu ngoặc kép ở cả hai đầu của chuỗi, chúng sẽ được cắt bỏ ngay lập tức. Hãy nhớ rằng nó chỉ loại bỏ các dấu ngoặc kép nếu chúng ở hai biên; dấu ngoặc kép nằm giữa văn bản của bạn sẽ vẫn còn đó.

## Xem thêm
Dành cho những tâm hồn tò mò muốn khám phá thêm:
- [Tài liệu về mô-đun String của Gleam](https://gleam.run/stdlib/string/)
- Các cuộc thảo luận về xử lý văn bản trong lập trình trên [Stack Overflow](https://stackoverflow.com/questions/tagged/text-processing)
