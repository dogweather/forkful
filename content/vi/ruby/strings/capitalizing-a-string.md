---
changelog:
- 2024-03-25, dogweather, edited and tested
- 2024-03-25, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:02:29.358527-07:00
description: "Vi\u1EC7c vi\u1EBFt hoa m\u1ED9t chu\u1ED7i th\u01B0\u1EDDng c\xF3 ngh\u0129\
  a l\xE0 chuy\u1EC3n k\xFD t\u1EF1 \u0111\u1EA7u ti\xEAn c\u1EE7a chu\u1ED7i th\xE0\
  nh ch\u1EEF in hoa v\xE0 ph\u1EA7n c\xF2n l\u1EA1i th\xE0nh ch\u1EEF th\u01B0\u1EDD\
  ng. Nh\u01B0ng \u0111\xF4i khi, n\xF3 c\xF3 th\u1EC3 ch\u1EC9\u2026"
lastmod: '2024-03-25T19:22:21.215761-06:00'
model: gpt-4-0125-preview
summary: "Vi\u1EC7c vi\u1EBFt hoa m\u1ED9t chu\u1ED7i th\u01B0\u1EDDng c\xF3 ngh\u0129\
  a l\xE0 chuy\u1EC3n k\xFD t\u1EF1 \u0111\u1EA7u ti\xEAn c\u1EE7a chu\u1ED7i th\xE0\
  nh ch\u1EEF in hoa v\xE0 ph\u1EA7n c\xF2n l\u1EA1i th\xE0nh ch\u1EEF th\u01B0\u1EDD\
  ng."
title: "Chuy\u1EC3n ch\u1EEF c\xE1i \u0111\u1EA7u ti\xEAn c\u1EE7a chu\u1ED7i th\xE0\
  nh ch\u1EEF hoa"
weight: 2
---

## Cái Gì và Tại Sao?
Việc viết hoa một chuỗi thường có nghĩa là chuyển ký tự đầu tiên của chuỗi thành chữ in hoa và phần còn lại thành chữ thường. Nhưng đôi khi, nó có thể chỉ đơn giản là đảm bảo ký tự đầu tiên là chữ in hoa trong khi để phần còn lại của chuỗi không thay đổi. Thành thật mà nói, theo ý kiến của tôi, đây là một thuật ngữ khá mơ hồ.

## Làm thế nào:
Ruby cung cấp [các phương thức đơn giản cho việc thao tác chuỗi](https://docs.ruby-lang.org/en/3.3/String.html), bao gồm cả việc viết hoa:

```ruby
# Phương thức có sẵn của Ruby
chuoi = "hello WORLD"
chuoi_viet_hoa = chuoi.capitalize
puts chuoi_viet_hoa # => "Hello world"
```

Rất tiện lợi.

Phương thức `.capitalize` của Ruby rất tiện dụng nhưng chỉ viết hoa chữ cái đầu tiên. Để có nhiều điều khiển hơn hoặc muốn viết hoa mỗi từ trong chuỗi (gọi là dạng tựa đề), bạn có thể muốn sử dụng phương thức `titleize` từ phần mở rộng ActiveSupport của Rails, hoặc tự triển khai nó:

```ruby
# Sử dụng 'titleize' của ActiveSupport trong Rails
require 'active_support/core_ext/string/inflections'
chuoi = "hello world"
puts chuoi.titleize # => "Hello World"
```

```ruby
# Một giải pháp tự làm
chuoi = "hello world"
vu_viet_hoa_moi_tu = chuoi.split.map(&:capitalize).join(' ')
puts vu_viet_hoa_moi_tu # => "Hello World"
```

Phương thức này chia chuỗi thành một mảng các từ, viết hoa từng từ, sau đó ghép chúng lại với một khoảng trắng.

Cá nhân tôi, tôi đưa ý tưởng này xa hơn nhiều trong mã của mình. Tôi đã viết phương thức [`titleize` của mình, mà xem xét đến những từ nhỏ như "a" và "the"](https://github.com/public-law/law_string/blob/master/lib/law_string.rb).
