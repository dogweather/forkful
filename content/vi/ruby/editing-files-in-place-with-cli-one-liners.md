---
title:                "Chỉnh sửa file tại chỗ với câu lệnh CLI ngắn gọn"
date:                  2024-01-28T22:00:51.728102-07:00
model:                 gpt-4-0125-preview
simple_title:         "Chỉnh sửa file tại chỗ với câu lệnh CLI ngắn gọn"

tag:                  "Data and Text Processing"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/ruby/editing-files-in-place-with-cli-one-liners.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cái gì & Tại sao?

Chỉnh sửa tập tin tại chỗ với CLI (Giao diện Dòng Lệnh) bằng một dòng lệnh trong Ruby cho phép bạn sửa đổi tập tin trực tiếp từ terminal của bạn, không cần mở chúng trong một trình soạn thảo, thực hiện thay đổi và lưu lại. Kỹ thuật này vô cùng hữu ích cho việc chỉnh sửa nhanh chóng, cập nhật hàng loạt, hoặc tự động hóa các tác vụ lặp đi lặp lại, tiết kiệm cả thời gian và công sức.

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
