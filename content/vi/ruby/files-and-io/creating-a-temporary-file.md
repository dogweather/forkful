---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:58:44.751962-07:00
description: "L\xE0m th\u1EBF n\xE0o: Th\u01B0 vi\u1EC7n chu\u1EA9n c\u1EE7a Ruby\
  \ cung c\u1EA5p `Tempfile` \u0111\u1EC3 t\u1EA1o t\u1EC7p t\u1EA1m. H\xE3y c\xF9\
  ng t\xECm hi\u1EC3u."
lastmod: '2024-03-13T22:44:37.366836-06:00'
model: gpt-4-0125-preview
summary: "Th\u01B0 vi\u1EC7n chu\u1EA9n c\u1EE7a Ruby cung c\u1EA5p `Tempfile` \u0111\
  \u1EC3 t\u1EA1o t\u1EC7p t\u1EA1m."
title: "T\u1EA1o m\u1ED9t t\u1EADp tin t\u1EA1m th\u1EDDi"
weight: 21
---

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
