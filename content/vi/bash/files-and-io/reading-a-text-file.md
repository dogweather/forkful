---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:04:48.138779-07:00
description: "\u0110\u1ECDc m\u1ED9t t\u1EC7p v\u0103n b\u1EA3n c\xF3 ngh\u0129a l\xE0\
  \ l\u1EA5y n\u1ED9i dung t\u1EEB t\u1EC7p v\xE0o trong k\u1ECBch b\u1EA3n (script)\
  \ c\u1EE7a b\u1EA1n. L\u1EADp tr\xECnh vi\xEAn th\u1EF1c hi\u1EC7n vi\u1EC7c n\xE0\
  y \u0111\u1EC3 l\xE0m vi\u1EC7c v\u1EDBi d\u1EEF li\u1EC7u, c\u1EA5u\u2026"
lastmod: '2024-03-13T22:44:36.901656-06:00'
model: gpt-4-0125-preview
summary: "\u0110\u1ECDc m\u1ED9t t\u1EC7p v\u0103n b\u1EA3n c\xF3 ngh\u0129a l\xE0\
  \ l\u1EA5y n\u1ED9i dung t\u1EEB t\u1EC7p v\xE0o trong k\u1ECBch b\u1EA3n (script)\
  \ c\u1EE7a b\u1EA1n."
title: "\u0110\u1ECDc m\u1ED9t t\u1EC7p v\u0103n b\u1EA3n"
weight: 22
---

## Cách thực hiện:
Dưới đây là cách đơn giản nhất để đọc một tệp dòng này qua dòng khác:

```Bash
while IFS= read -r line; do
    echo "Văn bản: $line"
done < "yourfile.txt"
```

Muốn lấy toàn bộ tệp một lần? Hãy thử cách này:

```Bash
file_content=$(<yourfile.txt)
echo "$file_content"
```

Hay bạn cần một dòng cụ thể, chẳng hạn dòng 4?

```Bash
sed '4q;d' yourfile.txt
```

Ví dụ về kết quả khi đọc dòng 4:

```
Đây là nội dung của dòng thứ tư.
```

## Sâu hơn
Ngày xưa, chúng ta không có các IDE lòe loẹt, chúng ta có các cửa sổ dòng lệnh và trình soạn thảo văn bản đơn giản. Các công cụ UNIX được thiết kế với triết lý làm một việc và làm tốt. `cat`, `less`, `sed` và `awk` là những lão làng trong việc thao tác với văn bản.

Đọc một tệp trong Bash tận dụng những công cụ này, cộng với việc chuyển hướng và vòng lặp của chính Bash. Ví dụ, sử dụng `while` với `read` rất tốt cho hiệu quả bộ nhớ khi làm việc với các tệp lớn. Bạn đang đọc từng dòng một, không đổ toàn bộ vào bộ nhớ.

`sed` là một trình biên tập dòng (stream editor). Lấy một dòng cụ thể với `sed '4q;d' yourfile.txt` bảo `sed` dừng lại sau dòng 4 (`4q`) và sau đó in (`;d`) dòng đó.

Có các lựa chọn khác. `awk` rất mạnh mẽ cho việc xử lý văn bản. Các kịch bản Perl và Python có thể được gọi trong Bash khi việc xử lý văn bản trở nên phức tạp. Mỗi công cụ và ngôn ngữ này có những trường hợp sử dụng và xem xét về hiệu năng riêng biệt của chúng.

## Xem thêm
1. Hướng dẫn Viết Kịch Bản Bash: https://www.gnu.org/software/bash/manual/
2. Mẹo với `sed` và `awk` 101: https://www.thegeekstuff.com/2009/12/unix-sed-tutorial-6-examples-to-edit-file-in-place/
3. Xử lý Văn bản Dòng Lệnh Linux với `grep`, `awk`, `sed`, `sort` và bạn bè: https://github.com/learnbyexample/Command-line-text-processing
