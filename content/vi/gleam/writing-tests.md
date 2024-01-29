---
title:                "Viết các bài kiểm tra"
date:                  2024-01-28T22:13:21.541204-07:00
model:                 gpt-4-0125-preview
simple_title:         "Viết các bài kiểm tra"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/gleam/writing-tests.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cái gì & Tại sao?

Viết kiểm thử là về việc tạo ra mã lệnh kiểm tra tính chính xác của mã lệnh khác. Lập trình viên làm điều này để bắt lỗi sớm, đảm bảo chất lượng, và bảo vệ khỏi việc thay đổi trong tương lai gây ra sự cố.

## Cách thực hiện:

```Gleam
import gleam/should
import my_module

pub fn my_test() {
  // Kiểm tra xem hàm trả về giá trị mong đợi hay không
  should.equal(my_module.my_function(), "kết quả mong đợi")
}

pub fn addition_test() {
  // Kiểm tra tính chính xác của hàm cộng
  should.equal(my_module.add(1, 2), 3)
}
```

Kết quả mẫu từ việc chạy bộ kiểm thử thành công:

```
Kiểm tra my_module...
  ✓ my_test passed
  ✓ addition_test passed

Tất cả các bài kiểm thử đã được vượt qua!
```

## Sâu hơn

Văn hóa kiểm thử của Gleam được lấy cảm hứng từ nguồn gốc Erlang của nó, nơi sự robust (độ bền vững) là điều quan trọng. Các phương thức thay thế như kiểm thử dựa trên thuộc tính cũng rất phổ biến trong hệ sinh thái Erlang. Về mặt thực hiện, kiểm thử trong Gleam chỉ là những hàm thông thường với các phát biểu xác nhận. Chúng được chạy bởi một bộ chạy kiểm thử và kết quả được báo cáo trong một định dạng dễ đọc.

## Xem thêm

- Kiểm thử chung của Erlang để tham khảo: [http://erlang.org/doc/apps/common_test/basics_chapter.html](http://erlang.org/doc/apps/common_test/basics_chapter.html)
