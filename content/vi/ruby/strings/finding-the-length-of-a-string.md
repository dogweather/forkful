---
title:                "Tìm chiều dài của một chuỗi ký tự"
aliases:
- /vi/ruby/finding-the-length-of-a-string.md
date:                  2024-01-28T22:00:48.474692-07:00
model:                 gpt-4-0125-preview
simple_title:         "Tìm chiều dài của một chuỗi ký tự"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/ruby/finding-the-length-of-a-string.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cái gì & Tại sao?
Tìm độ dài của một chuỗi nghĩa là đếm số ký tự của nó. Đây là một công việc cơ bản nhưng quan trọng cho các nhiệm vụ như xác thực, xử lý văn bản và xác định nhu cầu lưu trữ.

## Làm thế nào:
Ruby giữ nó đơn giản với phương thức `.length`:

```ruby
greeting = "Hello, world!"
puts greeting.length
```

Đầu ra:

```
13
```

Hoặc, sử dụng `.size` cũng có cùng tác dụng:

```ruby
greeting = "Hello, world!"
puts greeting.size
```

Đầu ra:

```
13
```

## Sâu hơn
Trong Ruby, `.length` và `.size` có thể thay thế cho nhau khi nói đến chuỗi; chúng đều cho bạn biết số ký tự. Cổ điển, Ruby đã tập trung vào việc làm cho mã nguồn tự nhiên hơn để đọc, đó là lý do tại sao bạn thường thấy có nhiều hơn một cách để làm cùng một việc.

Ở bên trong, mỗi ký tự trong một chuỗi ảnh hưởng đến kích thước lưu trữ. Vì vậy, việc biết được con số này có thể quan trọng cho tối ưu hóa, đặc biệt là với khối lượng văn bản lớn.

Trong khi `.length` và `.size` cho bạn biết số ký tự, trong một số ngôn ngữ và những thời điểm trước đây, độ dài của một chuỗi có thể được chỉ đến kích thước byte của nó. Tuy nhiên, Ruby với hỗ trợ ký tự đa byte thông qua Unicode, không đưa ra kích thước byte trực tiếp tương đương với độ dài chuỗi do ký tự có thể chiếm nhiều hơn một byte.

Các lựa chọn khác như `.bytesize` cho bạn biết một chuỗi chiếm bao nhiêu byte, và `.chars.count` cho bạn biết số ký tự bằng cách chuyển đổi chuỗi thành một mảng các ký tự trước.

Đây là cách bạn sử dụng `.bytesize` và `.chars.count`:

```ruby
greeting = "Hello, world!"
puts greeting.bytesize
puts greeting.chars.count
```

Đầu ra:

```
13
13
```

## Xem Thêm
- Tài liệu Ruby về Chuỗi: [https://ruby-doc.org/core/String.html](https://ruby-doc.org/core/String.html)
- Một bài giảng hay về Chuỗi Ruby bởi [RubyGuides](https://www.rubyguides.com/2018/01/ruby-string-methods/): khám phá thêm về những gì bạn có thể làm với chuỗi ngoài việc đo kích thước của chúng.
- Đào sâu vào mã hóa ký tự và cách nó ảnh hưởng đến các thao tác chuỗi với [bài viết này từ Thoughtbot](https://thoughtbot.com/blog/its-about-time-zones#character-encoding).
