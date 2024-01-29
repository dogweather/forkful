---
title:                "Ghi log"
date:                  2024-01-28T22:03:18.724775-07:00
model:                 gpt-4-0125-preview
simple_title:         "Ghi log"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/gleam/logging.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Gì và Tại Sao?
Ghi nhật ký cơ bản là cách chúng ta ghi lại những gì xảy ra trong chương trình của mình. Nó giống như có một hộp đen nhỏ; khi mọi thứ đi không đúng (và tin tôi, chắc chắn sẽ có), nhật ký là vô cùng quý giá để tìm ra điều gì đã xảy ra, chẩn đoán vấn đề và tối ưu hóa hiệu suất.

## Làm Thế Nào:
Trong Gleam, bạn thường sẽ tích hợp một thư viện ghi nhật ký—không có một cơ chế ghi nhật ký riêng biệt ngay từ đầu. Giả sử chúng ta đang sử dụng một crate giả định là `gleam_logger`. Đây là cách bạn có thể tích hợp nó:

```gleam
import gleam_logger

pub fn main() {
  gleam_logger.info("Ứng dụng đang khởi động!")
  let result = intense_calculation()

  case result {
    Ok(value) -> 
      gleam_logger.debug("Tính toán thành công", value)
    Error(err) -> 
      gleam_logger.error("Tính toán thất bại", err)
  }
}
```

Kết quả dự kiến trong nhật ký của bạn sẽ trông như thế này:

```
INFO: Ứng dụng đang khởi động!
DEBUG: Tính toán thành công 42
ERROR: Tính toán thất bại Lý do: Chia cho số không
```

## Sâu Hơn
Nghệ thuật của việc ghi nhật ký đã có từ những ngày đầu tiên của lập trình. Các nhà điều hành hệ thống thực sự nhận được nhật ký từ máy tính - đảm bảo mọi thứ chạy trơn tru. Tiến lên, ghi nhật ký đã đi vào số hóa, trở thành một phần cốt lõi của phát triển phần mềm.

Mặc dù Gleam, được một ngôn ngữ tương đối mới hướng đến hệ sinh thái Erlang, không có một khung ghi nhật ký được xây dựng sẵn, bạn có thể tận dụng các cơ sở ghi nhật ký Erlang đã trưởng thành hoặc các thư viện khác do cộng đồng cung cấp. Mỗi cái có những tính năng và điều kiện đánh đổi khác nhau: một số có thể cung cấp ghi nhật ký có cấu trúc, số khác thì dành cho việc xuất bản văn bản đơn giản.

Bây giờ, câu hỏi về việc triển khai một tiện ích ghi nhật ký: Nó có đơn giản không? Ở cái nhìn đầu tiên, có. Nhưng bóc tách các lớp bên dưới, bạn đang nhìn vào việc xử lý đồng thời, nghẽn I/O, quay vòng nhật ký, chuẩn hóa định dạng (nghĩ đến JSON cho ghi nhật ký có cấu trúc), lọc cấp độ, và có thể là theo dõi phân tán. Thêm vào đó, trong một nguyên tắc hàm, bạn nói chung muốn các phụ trợ (như ghi nhật ký) được xử lý theo cách dự đoán và kiểm soát được.

## Xem Thêm
Dưới đây là nơi bạn có thể tìm hiểu thêm về chi tiết của việc ghi nhật ký trong Gleam và hệ sinh thái xung quanh:
- [Tài liệu :logger của Erlang](http://erlang.org/doc/apps/kernel/logger_chapter.html): Vì Gleam biên dịch sang Erlang, nó được áp dụng trực tiếp.
- [Tài liệu thư viện tiêu chuẩn của Gleam](https://hexdocs.pm/gleam_stdlib/): Để cập nhật về bất kỳ tiện ích ghi nhật ký nào có thể được thêm vào.
- [Awesome Gleam](https://github.com/gleam-lang/awesome-gleam): Một danh sách tuyển chọn các nguồn lực, có thể bao gồm cả thư viện ghi nhật ký khi chúng trở nên có sẵn.
