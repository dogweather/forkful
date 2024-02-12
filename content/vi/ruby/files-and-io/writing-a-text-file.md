---
title:                "Viết một tệp văn bản"
aliases:
- /vi/ruby/writing-a-text-file.md
date:                  2024-01-28T22:12:56.202337-07:00
model:                 gpt-4-0125-preview
simple_title:         "Viết một tệp văn bản"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/ruby/writing-a-text-file.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Gì & Tại sao?

Viết vào một tệp văn bản trong Ruby nghĩa là lưu dữ liệu vào một tệp trên hệ thống của bạn. Các lập trình viên thực hiện điều này để duy trì dữ liệu, ghi nhật ký và chia sẻ dữ liệu giữa các chương trình khác nhau hoặc giữa các lần chạy chương trình.

## Làm thế nào:

Để viết vào một tệp văn bản trong Ruby, sử dụng lớp `File`. Dưới đây là một ví dụ nhanh:

```Ruby
File.open("output.txt", "w") do |file|
  file.puts "Xin chào, Ruby!"
end
```

Đầu ra mẫu (nội dung của `output.txt`):
```
Xin chào, Ruby!
```

Để thêm vào một tệp đã tồn tại, sử dụng chế độ "a":

```Ruby
File.open("output.txt", "a") do |file|
  file.puts "Thêm dòng này."
end
```

Đầu ra (nội dung bổ sung của `output.txt`):
```
Thêm dòng này.
```

## Tìm hiểu sâu hơn

Việc xử lý tệp của Ruby có nguồn gốc từ các thao tác I/O tệp của UNIX. Phương thức `open` có thể lấy một khối, tự động đóng tệp sau đó, điều này độc đáo và tiện lợi so với một số ngôn ngữ khác. Các phương thức thay thế cho `File.open` bao gồm `IO.write` cho các thao tác viết nhanh và các thư viện khác như `CSV` hoặc `FileUtils` cho các nhiệm vụ chuyên biệt.

Khi bạn viết vào một tệp, hãy lưu ý đến mã hóa ký tự và kết thúc dòng, đặc biệt khi tệp của bạn cần được đọc bởi các hệ thống hoặc ngôn ngữ khác nhau.

## Xem thêm

- Lớp IO của Ruby: https://ruby-doc.org/core/IO.html
- FileUtils của Ruby: https://ruby-doc.org/stdlib/libdoc/fileutils/rdoc/FileUtils.html
- Lớp File trong Ruby-Doc: https://ruby-doc.org/core/File.html
