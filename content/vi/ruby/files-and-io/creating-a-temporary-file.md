---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:58:44.751962-07:00
description: "Trong Ruby, vi\u1EC7c t\u1EA1o m\u1ED9t t\u1EC7p t\u1EA1m gi\xFAp b\u1EA3\
  o v\u1EC7 d\u1EEF li\u1EC7u nh\u1EA1y c\u1EA3m v\xE0 qu\u1EA3n l\xFD nhu c\u1EA7\
  u l\u01B0u tr\u1EEF tho\xE1ng qua. C\xE1c l\u1EADp tr\xECnh vi\xEAn s\u1EED d\u1EE5\
  ng n\xF3 cho vi\u1EC7c x\u1EED l\xFD t\u1EC7p ng\u1EAFn\u2026"
lastmod: '2024-03-13T22:44:37.366836-06:00'
model: gpt-4-0125-preview
summary: "Trong Ruby, vi\u1EC7c t\u1EA1o m\u1ED9t t\u1EC7p t\u1EA1m gi\xFAp b\u1EA3\
  o v\u1EC7 d\u1EEF li\u1EC7u nh\u1EA1y c\u1EA3m v\xE0 qu\u1EA3n l\xFD nhu c\u1EA7\
  u l\u01B0u tr\u1EEF tho\xE1ng qua."
title: "T\u1EA1o m\u1ED9t t\u1EADp tin t\u1EA1m th\u1EDDi"
weight: 21
---

## Cái gì và Tại sao?
Trong Ruby, việc tạo một tệp tạm giúp bảo vệ dữ liệu nhạy cảm và quản lý nhu cầu lưu trữ thoáng qua. Các lập trình viên sử dụng nó cho việc xử lý tệp ngắn hạn an toàn mà không làm bừa bãi hệ thống tệp.

## Làm thế nào:
Thư viện chuẩn của Ruby cung cấp `Tempfile` để tạo tệp tạm. Hãy cùng tìm hiểu:

```Ruby
require 'tempfile'

Tempfile.create('my_temp') do |tempfile|
  tempfile.write('Nội dung tạm thời')
  puts "Tệp tạm được đặt tại: #{tempfile.path}"
end
# Sau khi khối lệnh kết thúc, tệp sẽ được tự động xóa.
```

Khi bạn chạy đoạn mã này, bạn sẽ thấy:

```
Tệp tạm được đặt tại: /tmp/my_temp20180418-56789-1234567
```

Tệp này sẽ không tồn tại lâu hơn mức cần thiết. Ngay khi khối lệnh kết thúc, Ruby sẽ tự động dọn dẹp cho bạn.

## Sâu hơn
Lớp `Tempfile` đã có từ Ruby 1.8, được thực hành và tinh chỉnh theo thời gian. Bên dưới capô, nó sử dụng đường dẫn tệp tạm của hệ thống của bạn, do hệ điều hành cung cấp.

Có phương án khác không? Chắc chắn, bạn có thể tự tạo và theo dõi tệp tạm, nhưng tại sao lại phải tái phát minh bánh xe? `Tempfile` cung cấp cho bạn một tên tệp ngẫu nhiên, độc đáo, giảm thiểu nguy cơ xung đột.

Đối với những ai mong muốn kiểm soát nhiều hơn, phương thức `Tempfile.new` nhận các tham số để điều chỉnh tên tệp và vị trí. Nhưng nhớ rằng, có quyền năng lớn đi kèm với trách nhiệm lớn - bạn sẽ cần phải xóa những tệp này một cách thủ công.

Lợi ích thực sự khi sử dụng `Tempfile` nằm ở tính an toàn với luồng và thu gom rác tự động. Nó khóa tệp lại và đảm bảo rằng dữ liệu nhạy cảm không tồn tại lâu hơn nên thế. Một tệp tạm hoạt động giống như một đối tượng File thông thường, vì vậy bạn có thể đọc từ, viết vào, và thao tác với nó bằng các hoạt động tệp điển hình.

## Xem thêm
- Ruby API Dock cho các ví dụ sử dụng Tempfile sâu hơn: [API Dock Tempfile](https://apidock.com/ruby/Tempfile)
- Hướng dẫn về Ruby File I/O để biết thêm về cách xử lý tệp: [File I/O](https://www.rubyguides.com/2015/05/working-with-files-ruby/)
