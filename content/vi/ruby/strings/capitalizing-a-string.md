---
title:                "Viết hoa chuỗi ký tự"
date:                  2024-03-25T17:32:04.009200-06:00
model:                 gpt-4-0125-preview
changelog:
  - 2024-03-25, dogweather, edited and tested
  - 2024-03-25, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cái Gì & Tại Sao?
Việc viết hoa một chuỗi thường có nghĩa là chuyển ký tự đầu tiên của chuỗi thành chữ in hoa và phần còn lại thành chữ thường. Nhưng đôi khi nó có thể chỉ đơn giản là đảm bảo ký tự đầu tiên được viết hoa trong khi để phần còn lại của chuỗi không thay đổi. Thành thực mà nói, theo ý kiến của tôi, đây là một thuật ngữ hơi mơ hồ.

## Làm Thế Nào:
Ruby cung cấp [những phương thức trực tiếp để thao tác chuỗi](https://docs.ruby-lang.org/en/3.3/String.html), bao gồm cả việc viết hoa:

```ruby
# Phương thức có sẵn của Ruby
string = "hello WORLD"
capitalized_string = string.capitalize
puts capitalized_string # => "Hello world"
```

Rất tiện lợi.

Phương thức `.capitalize` của Ruby rất thuận tiện nhưng nó chỉ viết hoa chữ cái đầu tiên. Để kiểm soát nhiều hơn hoặc viết hoa từng từ trong một chuỗi (còn được biết đến là chữ cái đầu của từng từ viết hoa), bạn có thể muốn sử dụng phương thức `titleize` từ mở rộng ActiveSupport của Rails, hoặc tự triển khai nó:

```ruby
# Sử dụng 'titleize' của ActiveSupport trong Rails
require 'active_support/core_ext/string/inflections'
string = "hello world"
puts string.titleize # => "Hello World"
```

```ruby
# Một giải pháp tự làm
string = "hello world"
capitalized_each_word = string.split.map(&:capitalize).join(' ')
puts capitalized_each_word # => "Hello World"
```

Phương pháp này chia chuỗi thành một mảng các từ, viết hoa từng từ, sau đó nối chúng lại với nhau bằng một khoảng trắng.

Cá nhân tôi, tôi đã áp dụng ý tưởng này nhiều hơn trong mã của mình. Tôi đã viết phương thức [`titleize` của riêng mình, mà còn xem xét đến những từ nhỏ như "a" và "the"](https://github.com/public-law/law_string/blob/master/lib/law_string.rb).
