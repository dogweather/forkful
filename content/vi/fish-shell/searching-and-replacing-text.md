---
title:                "Tìm kiếm và thay thế văn bản"
date:                  2024-01-28T22:07:13.477596-07:00
model:                 gpt-4-0125-preview
simple_title:         "Tìm kiếm và thay thế văn bản"

category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/fish-shell/searching-and-replacing-text.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cái gì & Tại sao?
Tìm kiếm và thay thế văn bản là việc tìm các chuỗi cụ thể và thay thế chúng bằng thứ khác. Lập trình viên thực hiện việc này để cập nhật mã, sửa lỗi, hoặc để định dạng lại dữ liệu — đây là cách tiết kiệm thời gian lớn.

## Cách thực hiện:
Hãy thay đổi tất cả các trường hợp 'cat' thành 'dog' trong một chuỗi.

```Fish Shell
echo "One cat, two cats, three cats." | string replace -a 'cat' 'dog'
```
Đầu ra mẫu:
```
One dog, two dogs, three dogs.
```
Thay thế văn bản trong một tệp có tên `pets.txt`:

```Fish Shell
string replace -a 'cat' 'dog' < pets.txt > updated_pets.txt
```

Sử dụng biến cho các mẫu:

```Fish Shell
set old "cat"
set new "dog"
string replace -a $old $new < pets.txt > updated_pets.txt
```

## Sâu hơn
Tính năng tìm kiếm và thay thế đã có mặt trong các trình chỉnh sửa văn bản từ những ngày đầu. Hãy nghĩ đến `sed` cho việc chỉnh sửa dòng trong Unix — đó là điều cũ mà cool. Fish đưa điều này lên xa hơn, làm cho nó đơn giản hơn với lệnh `string`. Không còn những cơn đau đầu về regex nữa trừ khi bạn muốn. Các phương án khác? Chắc chắn rồi: `sed`, `awk`, kịch bản Perl, thậm chí là macro `vim`. Nhưng lệnh `string` của Fish thì tinh tế và ít gặp lỗi hơn cho các trường hợp phổ thông.

## Xem thêm:
- Tài liệu chính thức của Fish Shell về lệnh `string`: [fishshell.com/docs/current/cmds/string.html](https://fishshell.com/docs/current/cmds/string.html)
- Sed bằng ví dụ, Phần 1: [https://www.gnu.org/software/sed/manual/sed.html](https://www.gnu.org/software/sed/manual/sed.html)
- Lập trình ngôn ngữ AWK — Các hàm chuỗi: [https://www.gnu.org/software/gawk/manual/gawk.html#String-Functions](https://www.gnu.org/software/gawk/manual/gawk.html#String-Functions)
