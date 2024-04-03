---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:58:24.959713-07:00
description: "L\xE0m Nh\u01B0 Th\u1EBF N\xE0o: Trong Fish Shell, b\u1EA1n c\xF3 th\u1EC3\
  \ t\u1EA1o m\u1ED9t t\u1EC7p t\u1EA1m th\u1EDDi b\u1EB1ng c\xE1ch s\u1EED d\u1EE5\
  ng `mktemp`. \u0110\xE2y l\xE0 m\u1ED9t v\xED d\u1EE5 nhanh."
lastmod: '2024-03-13T22:44:37.236913-06:00'
model: gpt-4-0125-preview
summary: "Trong Fish Shell, b\u1EA1n c\xF3 th\u1EC3 t\u1EA1o m\u1ED9t t\u1EC7p t\u1EA1\
  m th\u1EDDi b\u1EB1ng c\xE1ch s\u1EED d\u1EE5ng `mktemp`."
title: "T\u1EA1o m\u1ED9t t\u1EC7p t\u1EA1m th\u1EDDi"
weight: 21
---

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
