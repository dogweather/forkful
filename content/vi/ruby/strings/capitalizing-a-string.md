---
aliases:
- /vi/ruby/capitalizing-a-string/
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:56:25.419629-07:00
description: "Vi\u1EC7c vi\u1EBFt hoa m\u1ED9t chu\u1ED7i ngh\u0129a l\xE0 l\xE0m\
  \ cho k\xFD t\u1EF1 \u0111\u1EA7u ti\xEAn vi\u1EBFt hoa v\xE0 ph\u1EA7n c\xF2n l\u1EA1\
  i vi\u1EBFt th\u01B0\u1EDDng. C\xE1c l\u1EADp tr\xECnh vi\xEAn th\u1EF1c hi\u1EC7\
  n \u0111i\u1EC1u n\xE0y \u0111\u1EC3 \u0111\u1ECBnh d\u1EA1ng \u0111\u1EA7u ra\u2026"
lastmod: 2024-02-18 23:08:51.263337
model: gpt-4-0125-preview
summary: "Vi\u1EC7c vi\u1EBFt hoa m\u1ED9t chu\u1ED7i ngh\u0129a l\xE0 l\xE0m cho\
  \ k\xFD t\u1EF1 \u0111\u1EA7u ti\xEAn vi\u1EBFt hoa v\xE0 ph\u1EA7n c\xF2n l\u1EA1\
  i vi\u1EBFt th\u01B0\u1EDDng. C\xE1c l\u1EADp tr\xECnh vi\xEAn th\u1EF1c hi\u1EC7\
  n \u0111i\u1EC1u n\xE0y \u0111\u1EC3 \u0111\u1ECBnh d\u1EA1ng \u0111\u1EA7u ra\u2026"
title: "Vi\u1EBFt hoa m\u1ED9t chu\u1ED7i"
---

{{< edit_this_page >}}

## Cái gì & Tại sao?

Việc viết hoa một chuỗi nghĩa là làm cho ký tự đầu tiên viết hoa và phần còn lại viết thường. Các lập trình viên thực hiện điều này để định dạng đầu ra cho nhất quán hoặc để đáp ứng một số tiêu chuẩn dữ liệu.

## Cách thực hiện:

Trong Ruby, bạn viết hoa một chuỗi với phương thức `.capitalize`:

```Ruby
puts "hello world".capitalize  # Đầu ra: "Hello world"
```

Để viết hoa tất cả các từ trong một chuỗi, sử dụng:

```Ruby
puts "hello world".split.map(&:capitalize).join(' ')  # Đầu ra: "Hello World"
```

Lưu ý rằng `.capitalize` chỉ ảnh hưởng đến từ đầu tiên:

```Ruby
puts "hello WORLD".capitalize  # Đầu ra: "Hello world"
```

## Đi sâu hơn

Việc viết hoa chuỗi đã trở nên cần thiết kể từ khi máy tính bắt đầu tương tác với con người. Nó đảm bảo các danh từ riêng và câu bắt đầu một cách chính xác, đáp ứng các tiêu chuẩn ngữ pháp.

Trong một số ngôn ngữ như Ruby, `.capitalize` được tích hợp sẵn. Ngôn ngữ khác cần các hàm tùy chỉnh hoặc thư viện. Phương thức trong Ruby cũng chuyển phần còn lại của chuỗi về dạng thường, điều này có thể thấy trong các ví dụ trên.

Một phương án thay thế trong Ruby là sử dụng phương thức `titleize` từ các phương thức `ActiveSupport::Inflector`, chủ yếu được sử dụng trong Rails:

```Ruby
require 'active_support/core_ext/string/inflector'
puts "hello world".titleize  # Đầu ra: "Hello World"
```

Tuy nhiên, `titleize` nặng hơn và không phải là một phần của thư viện chuẩn của Ruby.

Về mặt triển khai, khi bạn gọi `.capitalize`, Ruby tạo một chuỗi mới với ký tự đầu tiên được chuyển thành chữ hoa và phần còn lại thành chữ thường. Nó rất tiện lợi để đảm bảo định dạng nhất quán trong giao diện người dùng và xử lý dữ liệu.

## Xem thêm

- Tài liệu của Ruby về `.capitalize`: [Tài liệu Ruby - capitalize](https://ruby-doc.org/core-2.7.0/String.html#method-i-capitalize)
- Về `ActiveSupport::Inflector` và `titleize`: [API Dock - titleize](https://apidock.com/rails/String/titleize)
- Để tìm hiểu về các phương thức chuỗi khác của Ruby: [Tài liệu Ruby - String](https://ruby-doc.org/core-2.7.0/String.html)
