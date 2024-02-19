---
aliases:
- /vi/fish-shell/creating-a-temporary-file/
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:58:24.959713-07:00
description: "T\u1EA1o m\u1ED9t t\u1EC7p t\u1EA1m th\u1EDDi c\xF3 ngh\u0129a l\xE0\
  \ l\xE0m m\u1ED9t t\u1EC7p cho vi\u1EC7c s\u1EED d\u1EE5ng ng\u1EAFn h\u1EA1n. C\xE1\
  c l\u1EADp tr\xECnh vi\xEAn l\xE0m \u0111i\u1EC1u n\xE0y \u0111\u1EC3 l\u01B0u tr\u1EEF\
  \ d\u1EEF li\u1EC7u ch\u1EC9 c\u1EA7n thi\u1EBFt trong qu\xE1 tr\xECnh\u2026"
lastmod: 2024-02-18 23:08:51.207818
model: gpt-4-0125-preview
summary: "T\u1EA1o m\u1ED9t t\u1EC7p t\u1EA1m th\u1EDDi c\xF3 ngh\u0129a l\xE0 l\xE0\
  m m\u1ED9t t\u1EC7p cho vi\u1EC7c s\u1EED d\u1EE5ng ng\u1EAFn h\u1EA1n. C\xE1c l\u1EAD\
  p tr\xECnh vi\xEAn l\xE0m \u0111i\u1EC1u n\xE0y \u0111\u1EC3 l\u01B0u tr\u1EEF d\u1EEF\
  \ li\u1EC7u ch\u1EC9 c\u1EA7n thi\u1EBFt trong qu\xE1 tr\xECnh\u2026"
title: "T\u1EA1o m\u1ED9t t\u1EC7p t\u1EA1m th\u1EDDi"
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
