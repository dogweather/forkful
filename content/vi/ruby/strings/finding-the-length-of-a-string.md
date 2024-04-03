---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:00:48.474692-07:00
description: "T\xECm \u0111\u1ED9 d\xE0i c\u1EE7a m\u1ED9t chu\u1ED7i ngh\u0129a l\xE0\
  \ \u0111\u1EBFm s\u1ED1 k\xFD t\u1EF1 c\u1EE7a n\xF3. \u0110\xE2y l\xE0 m\u1ED9\
  t c\xF4ng vi\u1EC7c c\u01A1 b\u1EA3n nh\u01B0ng quan tr\u1ECDng cho c\xE1c nhi\u1EC7\
  m v\u1EE5 nh\u01B0 x\xE1c th\u1EF1c, x\u1EED l\xFD v\u0103n b\u1EA3n v\xE0 x\xE1\
  c\u2026"
lastmod: '2024-03-13T22:44:37.325144-06:00'
model: gpt-4-0125-preview
summary: "T\xECm \u0111\u1ED9 d\xE0i c\u1EE7a m\u1ED9t chu\u1ED7i ngh\u0129a l\xE0\
  \ \u0111\u1EBFm s\u1ED1 k\xFD t\u1EF1 c\u1EE7a n\xF3."
title: "T\xECm chi\u1EC1u d\xE0i c\u1EE7a m\u1ED9t chu\u1ED7i k\xFD t\u1EF1"
weight: 7
---

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
