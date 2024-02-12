---
title:                "Tạo một tệp tạm thời"
aliases:
- /vi/fish-shell/creating-a-temporary-file/
date:                  2024-01-28T21:58:24.959713-07:00
model:                 gpt-4-0125-preview
simple_title:         "Tạo một tệp tạm thời"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/fish-shell/creating-a-temporary-file.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cái Gì & Tại Sao?

Tạo một tệp tạm thời có nghĩa là làm một tệp cho việc sử dụng ngắn hạn. Các lập trình viên làm điều này để lưu trữ dữ liệu chỉ cần thiết trong quá trình thực thi chương trình, như kết quả trung gian hoặc để đảm bảo một trạng thái sạch sẽ mà không làm lộn xộn bộ nhớ lưu trữ vĩnh viễn.

## Làm Như Thế Nào:

Trong Fish Shell, bạn có thể tạo một tệp tạm thời bằng cách sử dụng `mktemp`. Đây là một ví dụ nhanh:

```fish
set tempfile (mktemp)
echo "Hello, temporary world!" > $tempfile
cat $tempfile
rm $tempfile
```

Và bạn sẽ thấy điều gì đó như thế này:

```shell
Hello, temporary world!
```

Điều này tạo một tệp tạm thời, viết một dòng vào nó, hiển thị nội dung, và sau đó xóa tệp.

## Đào Sâu Hơn

Ngày xưa, các tệp tạm thời thường được tạo một cách thủ công, dẫn đến tiềm năng xung đột tên và vấn đề an ninh. `mktemp` đến để giải cứu! Lệnh này tạo một tệp với một tên duy nhất, giảm nguy cơ va chạm tệp.

Các phương pháp khác bao gồm viết vào `/dev/shm` trên Linux hoặc sử dụng các hệ thống tệp dựa trên bộ nhớ. Tuy nhiên, các phương pháp này không linh hoạt bằng `mktemp`.

Về thời hạn tồn tại của các tệp tạm thời, điều quan trọng cần nhớ là chúng nên được xóa bởi chương trình tạo ra chúng. Điều này đảm bảo không để lại các tệp gây lãng phí không gian hệ thống. Trong một số hệ thống, thư mục `/tmp` được dọn sạch khi khởi động lại, nhưng bạn không nên dựa vào hành vi này để dọn dẹp.

## Xem Thêm

- Tài liệu Fish Shell: [https://fishshell.com/docs/current/](https://fishshell.com/docs/current/)
- Hướng dẫn `mktemp`: [https://www.gnu.org/software/autogen/mktemp.html](https://www.gnu.org/software/autogen/mktemp.html)
- Chuẩn Hệ Thống Phân Lớp Tệp: [https://refspecs.linuxfoundation.org/FHS_3.0/fhs/index.html](https://refspecs.linuxfoundation.org/FHS_3.0/fhs/index.html)
