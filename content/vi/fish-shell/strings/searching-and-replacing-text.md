---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:07:13.477596-07:00
description: "T\xECm ki\u1EBFm v\xE0 thay th\u1EBF v\u0103n b\u1EA3n l\xE0 vi\u1EC7\
  c t\xECm c\xE1c chu\u1ED7i c\u1EE5 th\u1EC3 v\xE0 thay th\u1EBF ch\xFAng b\u1EB1\
  ng th\u1EE9 kh\xE1c. L\u1EADp tr\xECnh vi\xEAn th\u1EF1c hi\u1EC7n vi\u1EC7c n\xE0\
  y \u0111\u1EC3 c\u1EADp nh\u1EADt m\xE3, s\u1EEDa l\u1ED7i, ho\u1EB7c\u2026"
lastmod: '2024-02-25T18:49:35.536108-07:00'
model: gpt-4-0125-preview
summary: "T\xECm ki\u1EBFm v\xE0 thay th\u1EBF v\u0103n b\u1EA3n l\xE0 vi\u1EC7c t\xEC\
  m c\xE1c chu\u1ED7i c\u1EE5 th\u1EC3 v\xE0 thay th\u1EBF ch\xFAng b\u1EB1ng th\u1EE9\
  \ kh\xE1c. L\u1EADp tr\xECnh vi\xEAn th\u1EF1c hi\u1EC7n vi\u1EC7c n\xE0y \u0111\
  \u1EC3 c\u1EADp nh\u1EADt m\xE3, s\u1EEDa l\u1ED7i, ho\u1EB7c\u2026"
title: "T\xECm ki\u1EBFm v\xE0 thay th\u1EBF v\u0103n b\u1EA3n"
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
