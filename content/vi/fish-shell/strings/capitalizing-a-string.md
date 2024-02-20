---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:55:44.375823-07:00
description: "Vi\u1EC7c vi\u1EBFt hoa m\u1ED9t chu\u1ED7i c\xF3 ngh\u0129a l\xE0 chuy\u1EC3\
  n t\u1EA5t c\u1EA3 c\xE1c k\xFD t\u1EF1 th\xE0nh ch\u1EEF hoa. L\u1EADp tr\xECnh\
  \ vi\xEAn th\u1EF1c hi\u1EC7n vi\u1EC7c n\xE0y \u0111\u1EC3 \u0111\u1EA3m b\u1EA3\
  o t\xEDnh nh\u1EA5t qu\xE1n, d\u1EC5 \u0111\u1ECDc ho\u1EB7c \u0111\u1EC3 \u0111\
  \xE1p\u2026"
lastmod: 2024-02-19 22:04:56.417759
model: gpt-4-0125-preview
summary: "Vi\u1EC7c vi\u1EBFt hoa m\u1ED9t chu\u1ED7i c\xF3 ngh\u0129a l\xE0 chuy\u1EC3\
  n t\u1EA5t c\u1EA3 c\xE1c k\xFD t\u1EF1 th\xE0nh ch\u1EEF hoa. L\u1EADp tr\xECnh\
  \ vi\xEAn th\u1EF1c hi\u1EC7n vi\u1EC7c n\xE0y \u0111\u1EC3 \u0111\u1EA3m b\u1EA3\
  o t\xEDnh nh\u1EA5t qu\xE1n, d\u1EC5 \u0111\u1ECDc ho\u1EB7c \u0111\u1EC3 \u0111\
  \xE1p\u2026"
title: "Vi\u1EBFt hoa m\u1ED9t chu\u1ED7i"
---

{{< edit_this_page >}}

## Gì & Tại Sao?
Việc viết hoa một chuỗi có nghĩa là chuyển tất cả các ký tự thành chữ hoa. Lập trình viên thực hiện việc này để đảm bảo tính nhất quán, dễ đọc hoặc để đáp ứng một số tiêu chuẩn viết mã nào đó.

## Làm thế nào:
Trong Fish, bạn viết hoa một chuỗi bằng lệnh `string upper`. Dưới đây là cách bạn thực hiện:

```Fish Shell
set lowercased "fish shell is fun"
set capitalized (string upper $lowercased)
echo $capitalized
```

Kết quả:
```
FISH SHELL IS FUN
```

## Tìm Hiểu Sâu Hơn
Trong lịch sử, việc viết hoa chuỗi trong lập trình được sử dụng để định dạng đầu ra, lưu trữ dữ liệu một cách thống nhất, và cho việc so sánh không phân biệt chữ hoa chữ thường. Mặc dù Fish Shell còn khá mới, nhưng các chức năng biểu diễn chuỗi của nó lấy cảm hứng từ các shell Unix khác, mang lại cú pháp dễ đọc và tiện lợi hơn.

Các điểm chính trong triết lý thiết kế của Fish bao gồm sự thân thiện với người dùng và cung cấp chức năng theo đúng như bạn mong đợi, do đó có lệnh `string upper` dễ hiểu. Các shell trước đây yêu cầu bạn phải dùng lệnh echo kết hợp với `tr` hoặc sử dụng `awk` cho loại thao tác này, có thể kém trực quan đối với người dùng thông thường.

Các phương án thay thế bao gồm sử dụng `awk`:
```Fish Shell
echo "fish shell is fun" | awk '{print toupper($0)}'
```

Hoặc `tr`:
```Fish Shell
echo "fish shell is fun" | tr '[:lower:]' '[:upper:]'
```

Mặc dù có những phương án thay thế này, `string upper` trong Fish rõ ràng và đi thẳng vào vấn đề, tránh được gánh nặng lịch sử của Unix với các tùy chọn lệnh và cú pháp khó hiểu. Việc viết hoa một chuỗi trong Fish không thay đổi chuỗi gốc trừ khi bạn rõ ràng gán lại nó, bảo vệ dữ liệu của bạn khỏi các biến đổi không mong muốn.

## Xem Thêm
- Tài liệu về việc biểu diễn chuỗi trong Fish: [fishshell.com/docs/current/cmds/string.html](https://fishshell.com/docs/current/cmds/string.html)
- Một lịch sử ngắn về các công cụ xử lý văn bản trong Unix: [Unix Text Processing (O'Reilly)](http://www.oreilly.com)
- Hướng dẫn về việc biểu diễn chuỗi trong các shell Unix để so sánh: [Greg's Wiki (mywiki.wooledge.org)](http://mywiki.wooledge.org/BashFAQ/099)
