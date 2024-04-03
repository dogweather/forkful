---
changelog:
- 2024-03-25, dogweather, edited and tested
- 2024-03-25, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:02:29.358527-07:00
description: "L\xE0m th\u1EBF n\xE0o: Ruby cung c\u1EA5p [c\xE1c ph\u01B0\u01A1ng\
  \ th\u1EE9c \u0111\u01A1n gi\u1EA3n cho vi\u1EC7c thao t\xE1c chu\u1ED7i](https://docs.ruby-lang.org/en/3.3/String.html),\
  \ bao g\u1ED3m c\u1EA3 vi\u1EC7c vi\u1EBFt hoa."
lastmod: '2024-03-25T19:22:21.215761-06:00'
model: gpt-4-0125-preview
summary: "Ruby cung c\u1EA5p [c\xE1c ph\u01B0\u01A1ng th\u1EE9c \u0111\u01A1n gi\u1EA3\
  n cho vi\u1EC7c thao t\xE1c chu\u1ED7i](https://docs.ruby-lang.org/en/3.3/String.html),\
  \ bao g\u1ED3m c\u1EA3 vi\u1EC7c vi\u1EBFt hoa."
title: "Chuy\u1EC3n ch\u1EEF c\xE1i \u0111\u1EA7u ti\xEAn c\u1EE7a chu\u1ED7i th\xE0\
  nh ch\u1EEF hoa"
weight: 2
---

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
