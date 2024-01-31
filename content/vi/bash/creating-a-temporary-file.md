---
title:                "Tạo một tập tin tạm thời"
date:                  2024-01-28T21:58:37.854650-07:00
model:                 gpt-4-0125-preview
simple_title:         "Tạo một tập tin tạm thời"

category:             "Bash"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/bash/creating-a-temporary-file.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cái gì & Tại sao?
Tạo một tập tin tạm thời trong Bash có nghĩa là tạo một tập tin mà các kịch bản của bạn có thể sử dụng để lưu trữ dữ liệu một cách ngắn gọn. Lập trình viên làm điều này để lưu giữ các thông tin nhỏ trong khi thực hiện các nhiệm vụ phức tạp, tránh làm đầy ổ cứng, và để giảm thiểu xung đột giữa các quá trình khác nhau cố gắng sử dụng cùng một tập tin.

## Làm thế nào:
Bash có một lệnh được tích hợp sẵn gọi là `mktemp` để dễ dàng tạo tập tin tạm thời:

```Bash
# Tạo một tập tin tạm thời
temp_file=$(mktemp)

# Xem xét tập tin tạm thời mới của chúng tôi
echo "Tập tin tạm thời đã được tạo: $temp_file"

# Sử dụng tập tin tạm thời
echo "Một số dữ liệu" > "$temp_file"

# Đọc lại nó
cat "$temp_file"

# Dọn dẹp: xóa tập tin khi bạn hoàn thành
rm "$temp_file"
```
Đầu ra:
```
Tập tin tạm thời đã được tạo: /tmp/tmp.Iy5nv69sed
Một số dữ liệu
```

## Sâu hơn
Tập tin tạm thời đã có trong UNIX từ những ngày đầu, cho phép người dùng xử lý dữ liệu trung gian mà không cần dọn dẹp thủ công. Trong kịch bản Bash, `mktemp` là cách tiếp cận hiện đại, đi kèm với các tùy chọn để tạo cả tập tin (`mktemp`) và thư mục (`mktemp -d`). Lệnh này tạo một tập tin duy nhất mỗi lần nó được gọi, điều này né tránh vấn đề xung đột tập tin xảy ra khi nhiều phiên bản của một kịch bản hoặc các kịch bản khác nhau đang chạy cùng một lúc.

Trước `mktemp`, các lập trình viên thường tạo tập tin thủ công với tên mà họ hy vọng sẽ là duy nhất. Xung đột là chuyện thường xảy ra, dẫn đến mất dữ liệu và vấn đề về bảo mật. `mktemp` giúp ngăn chặn điều đó bằng cách đảm bảo rằng tên tập tin là duy nhất với sự kết hợp của các mẫu dự đoán và ký tự ngẫu nhiên. Không giống như các tập tin thường, những tập tin tạm thời này được dự định để được xóa sau khi sử dụng, giữ cho hệ thống gọn gàng.

Một số phương án thay thế cho `mktemp` bao gồm sử dụng `/dev/shm` cho các tập tin tạm thời trong bộ nhớ, hoặc tạo ra một tập tin với ngày tháng và ID quá trình (`$$`), nhưng những phương pháp này đi kèm với nhiều rủi ro xung đột hơn.

## Xem Thêm
- Trang man cho mktemp: chạy `man mktemp` trong Bash.
- [Hướng dẫn GNU Coreutils](https://www.gnu.org/software/coreutils/manual/coreutils.html): để biết chi tiết về các lệnh chuẩn GNU/Linux.
- [Hướng dẫn Scripting Bash Nâng cao](https://www.tldp.org/LDP/abs/html/): để biết thêm về các kỹ thuật và ví dụ scripting phức tạp.
