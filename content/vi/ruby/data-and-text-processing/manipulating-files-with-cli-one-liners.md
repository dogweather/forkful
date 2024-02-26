---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:03:44.259186-07:00
description: "Thao t\xE1c v\u1EDBi t\u1EC7p b\u1EB1ng d\xF2ng l\u1EC7nh CLI trong\
  \ Ruby l\xE0 vi\u1EC7c th\u1EF1c hi\u1EC7n c\xE1c thao t\xE1c t\u1EC7p th\xF4ng\
  \ th\u01B0\u1EDDng tr\u1EF1c ti\u1EBFp t\u1EEB terminal c\u1EE7a b\u1EA1n s\u1EED\
  \ d\u1EE5ng c\xE1c script Ruby. \u0110\xE2y\u2026"
lastmod: '2024-02-25T18:49:35.668871-07:00'
model: gpt-4-0125-preview
summary: "Thao t\xE1c v\u1EDBi t\u1EC7p b\u1EB1ng d\xF2ng l\u1EC7nh CLI trong Ruby\
  \ l\xE0 vi\u1EC7c th\u1EF1c hi\u1EC7n c\xE1c thao t\xE1c t\u1EC7p th\xF4ng th\u01B0\
  \u1EDDng tr\u1EF1c ti\u1EBFp t\u1EEB terminal c\u1EE7a b\u1EA1n s\u1EED d\u1EE5\
  ng c\xE1c script Ruby. \u0110\xE2y\u2026"
title: "Thao t\xE1c v\u1EDBi c\xE1c t\u1EC7p tin b\u1EB1ng c\xE1c l\u1EC7nh CLI ch\u1EC9\
  \ m\u1ED9t d\xF2ng"
---

{{< edit_this_page >}}

## Cái gì & Tại sao?

Thao tác với tệp bằng dòng lệnh CLI trong Ruby là việc thực hiện các thao tác tệp thông thường trực tiếp từ terminal của bạn sử dụng các script Ruby. Đây là phương pháp mạnh mẽ để tự động hóa và nhanh chóng thực hiện các nhiệm vụ liên quan đến tệp, giúp tiết kiệm thời gian quý báu của lập trình viên và giảm thiểu khả năng xảy ra lỗi do thao tác thủ công.

## Làm thế nào:

Ruby, với cú pháp biểu cảm của mình, cho phép viết các dòng lệnh ngắn gọn và dễ đọc có thể xử lý đa dạng các thao tác tệp. Dưới đây là một số ví dụ bạn có thể thấy hữu ích:

**Đọc một tệp**

```ruby
ruby -e 'puts File.read("example.txt")'
```

Dòng lệnh này đọc và in nội dung của 'example.txt'. Đơn giản, nhưng hiệu quả để nhanh chóng xem qua các tệp.

**Thêm vào một tệp**

```ruby
ruby -e 'File.open("example.txt", "a") { |f| f.puts "New line" }'
```

Thêm một dòng mới vào 'example.txt' mà không cần phải mở nó trong một trình soạn thảo. Tuyệt vời cho việc ghi log hoặc cập nhật các tệp ngay lập tức.

**Đổi tên một tệp**

```ruby
ruby -e 'File.rename("example.txt", "new_example.txt")'
```

Đổi tên tệp từ 'example.txt' sang 'new_example.txt'. Một cách nhanh chóng để tổ chức hoặc sửa đổi tên tệp mà không cần trình quản lý tệp đồ họa.

**Xóa một tệp**

```ruby
ruby -e 'File.delete("unnecessary_file.txt")'
```

Khi bạn cần dọn dẹp và xóa bỏ các tệp, đây là dòng lệnh để bạn sử dụng.

Mặc dù các ví dụ đã trình bày sự dễ dàng mà Ruby có thể thao tác với tệp từ CLI, nhưng đó là điều quan trọng khi xử lý các thao tác tệp cần thận trọng để tránh mất dữ liệu do tai nạn. Luôn sao lưu dữ liệu quan trọng trước khi thực hiện các thao tác huỷ diệt như xóa hoặc ghi đè.

## Sâu hơn nữa

Thao tác tệp với dòng lệnh Ruby không phải là đặc quyền riêng của Ruby; các ngôn ngữ như Perl và Awk đã được sử dụng cho các nhiệm vụ tương tự trong hàng thập kỷ. Ruby, tuy nhiên, kết hợp sức mạnh biểu đạt của Perl với tính dễ đọc, làm cho việc tạo script trở nên trực quan hơn. Tuy nhiên, một điểm yếu của Ruby trong thao tác tệp CLI có thể là hiệu suất, đặc biệt khi xử lý với các tệp lớn hoặc các thao tác phức tạp - ngôn ngữ script nói chung chậm hơn so với ngôn ngữ biên dịch hoặc các công cụ Unix chuyên dụng như `sed` hoặc `awk` cho các nhiệm vụ xử lý văn bản.

Mặc dù vậy, các script Ruby cực kỳ linh hoạt và có thể dễ dàng tích hợp vào các ứng dụng Ruby lớn hơn hoặc các dự án Rails. Sự dễ đọc của chúng và các chức năng rộng lớn được cung cấp qua thư viện tiêu chuẩn và các gem khiến Ruby trở thành lựa chọn vững chắc cho các nhà phát triển tìm kiếm sự cân bằng giữa hiệu suất và năng suất.

Các lựa chọn thay thế cho thao tác tệp bao gồm sử dụng lệnh Unix/Linux gốc, Perl hoặc Python. Mỗi lựa chọn có điểm mạnh của mình; ví dụ, các lệnh Unix về hiệu suất không thể bị đánh bại cho các nhiệm vụ đơn giản, Python cân bằng giữa sự dễ đọc và hiệu quả, và Perl vẫn là một cường quốc cho xử lý văn bản. Sự lựa chọn thường dựa trên sở thích cá nhân, độ phức tạp của nhiệm vụ, và môi trường mà các script sẽ được thực hiện.

Việc hiểu các lựa chọn thay thế và bối cảnh lịch sử của thao tác tệp trong lập trình làm giàu thêm sự đánh giá của chúng ta về vị trí của Ruby trong phát triển hiện đại, nhận ra cả điểm mạnh và những lĩnh vực mà công cụ khác có thể phù hợp hơn.
