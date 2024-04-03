---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:00:51.728102-07:00
description: "L\xE0m th\u1EBF n\xE0o: Ruby cung c\u1EA5p m\u1ED9t c\xE1ch \u0111\u01A1\
  n gi\u1EA3n \u0111\u1EC3 ch\u1EC9nh s\u1EEDa t\xE0i li\u1EC7u ngay t\u1EA1i ch\u1ED7\
  \ t\u1EEB d\xF2ng l\u1EC7nh. S\u1EED d\u1EE5ng c\xF4ng t\u1EAFc `-i` c\u1EE7a Ruby,\
  \ b\u1EA1n c\xF3 th\u1EC3 b\u1EA3o Ruby ho\u1EA1t\u2026"
lastmod: '2024-03-13T22:44:37.334582-06:00'
model: gpt-4-0125-preview
summary: "Ruby cung c\u1EA5p m\u1ED9t c\xE1ch \u0111\u01A1n gi\u1EA3n \u0111\u1EC3\
  \ ch\u1EC9nh s\u1EEDa t\xE0i li\u1EC7u ngay t\u1EA1i ch\u1ED7 t\u1EEB d\xF2ng l\u1EC7\
  nh."
title: "Ch\u1EC9nh s\u1EEDa file t\u1EA1i ch\u1ED7 v\u1EDBi c\xE2u l\u1EC7nh CLI ng\u1EAF\
  n g\u1ECDn"
weight: 32
---

## Làm thế nào:
Ruby cung cấp một cách đơn giản để chỉnh sửa tài liệu ngay tại chỗ từ dòng lệnh. Sử dụng công tắc `-i` của Ruby, bạn có thể bảo Ruby hoạt động trực tiếp trên tập tin (các tập tin) đã cung cấp. Hãy thử với một vài ví dụ để xem cách này hoạt động như thế nào trong thực tế. Hãy tưởng tượng bạn có một tập tin `greetings.txt` với nội dung sau:

```
Hello, world!
Hello, Ruby!
Hello, programming!
```

Và bạn muốn thay thế từ "Hello" bằng "Hi". Đây là cách bạn có thể làm điều đó:

```Ruby
ruby -i -pe "gsub(/Hello/, 'Hi')" greetings.txt
```

Sau khi chạy lệnh này, `greetings.txt` sẽ được cập nhật thành:

```
Hi, world!
Hi, Ruby!
Hi, programming!
```

Nếu bạn lo lắng về việc có thể làm hỏng dữ liệu, Ruby đã có cách bảo vệ bạn. Bằng cách cung cấp một phần mở rộng cho công tắc `-i`, Ruby tạo một bản sao lưu trước khi thực hiện những thay đổi. Chẳng hạn:

```Ruby
ruby -i.bak -pe "gsub(/Hello/, 'Bye')" greetings.txt
```

Bây giờ, cùng với `greetings.txt` đã chỉnh sửa của bạn, bạn sẽ tìm thấy một `greetings.txt.bak` trong cùng một thư mục, giữ lại nội dung gốc.

## Sâu hơn nữa
Sức mạnh của chỉnh sửa tập tin tại chỗ trong Ruby đến từ sự kết hợp của khả năng xử lý văn bản giống như Perl và vẻ đẹp cú pháp đặc trưng của Ruby. Về mặt lịch sử, Perl là ngôn ngữ đi đến cho việc viết kịch bản một dòng ngắn gọn, đặc biệt là cho việc thao tác văn bản. Ruby đã tiếp nhận lối tư duy này, cho phép khả năng viết kịch bản dòng lệnh mạnh mẽ.

Những lựa chọn khác cho việc chỉnh sửa tại chỗ tồn tại trong các ngôn ngữ khác, chẳng hạn như chính Perl và sed, một trình biên tập luồng trong hệ thống Unix. Mỗi ngôn ngữ đều có điểm mạnh của mình—Perl được biết đến với khả năng xử lý văn bản của mình trong khi sed không ai sánh kịp về độ đơn giản cho các tác vụ biên tập luồng. Tuy nhiên, Ruby cung cấp một sự cân bằng, mang lại khả năng thao tác văn bản mạnh mẽ với cú pháp dễ đọc và thân thiện với người dùng hơn, đặc biệt là đối với những người đã quen thuộc với Ruby.

Về mặt triển khai, chỉnh sửa tại chỗ trong Ruby hoạt động bằng cách đổi tên tập tin gốc, tạo một tập tin mới với tên tập tin gốc và sau đó viết những thay đổi vào tập tin mới này khi nó đọc từ tập tin đã được đổi tên gốc. Cách tiếp cận này đảm bảo tính nguyên tử của hoạt động; toàn bộ tập tin được xử lý thành công hoặc không có thay đổi nào được thực hiện, bảo vệ tính toàn vẹn của dữ liệu của bạn trong quá trình chỉnh sửa. Cơ chế này, kết hợp với việc xử lý ngoại lệ của Ruby, cũng cung cấp sự chống chịu đối với các gián đoạn, chẳng hạn như sự cố mất điện hoặc kết thúc quá trình, đảm bảo rằng ít nhất bản sao lưu vẫn còn nguyên vẹn.

Tóm lại, việc chỉnh sửa tập tin tại chỗ của Ruby là bằng chứng về sự tiện ích của nó như một ngôn ngữ kịch bản, cung cấp một sự kết hợp của sức mạnh, sự đơn giản, và vẻ đẹp cho các tác vụ thao tác văn bản trực tiếp từ dòng lệnh.
