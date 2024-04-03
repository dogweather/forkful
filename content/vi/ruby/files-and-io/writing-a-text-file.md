---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:12:56.202337-07:00
description: "L\xE0m th\u1EBF n\xE0o: \u0110\u1EC3 vi\u1EBFt v\xE0o m\u1ED9t t\u1EC7\
  p v\u0103n b\u1EA3n trong Ruby, s\u1EED d\u1EE5ng l\u1EDBp `File`. D\u01B0\u1EDB\
  i \u0111\xE2y l\xE0 m\u1ED9t v\xED d\u1EE5 nhanh."
lastmod: '2024-03-13T22:44:37.365573-06:00'
model: gpt-4-0125-preview
summary: "\u0110\u1EC3 vi\u1EBFt v\xE0o m\u1ED9t t\u1EC7p v\u0103n b\u1EA3n trong\
  \ Ruby, s\u1EED d\u1EE5ng l\u1EDBp `File`."
title: "Vi\u1EBFt m\u1ED9t t\u1EC7p v\u0103n b\u1EA3n"
weight: 24
---

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
