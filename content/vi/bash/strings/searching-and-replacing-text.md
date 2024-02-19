---
aliases:
- /vi/bash/searching-and-replacing-text/
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:07:20.661159-07:00
description: "T\xECm ki\u1EBFm v\xE0 thay th\u1EBF v\u0103n b\u1EA3n trong bash c\xF3\
  \ ngh\u0129a l\xE0 thay th\u1EBF t\u1EEB ho\u1EB7c m\u1EABu trong m\u1ED9t chu\u1ED7\
  i ho\u1EB7c t\u1EC7p b\u1EB1ng th\u1EE9 g\xEC \u0111\xF3 kh\xE1c. \u0110\xE2y l\xE0\
  \ m\u1ED9t nhi\u1EC7m v\u1EE5 h\xE0ng ng\xE0y cho\u2026"
lastmod: 2024-02-18 23:08:50.866704
model: gpt-4-0125-preview
summary: "T\xECm ki\u1EBFm v\xE0 thay th\u1EBF v\u0103n b\u1EA3n trong bash c\xF3\
  \ ngh\u0129a l\xE0 thay th\u1EBF t\u1EEB ho\u1EB7c m\u1EABu trong m\u1ED9t chu\u1ED7\
  i ho\u1EB7c t\u1EC7p b\u1EB1ng th\u1EE9 g\xEC \u0111\xF3 kh\xE1c. \u0110\xE2y l\xE0\
  \ m\u1ED9t nhi\u1EC7m v\u1EE5 h\xE0ng ng\xE0y cho\u2026"
title: "T\xECm ki\u1EBFm v\xE0 thay th\u1EBF v\u0103n b\u1EA3n"
---

{{< edit_this_page >}}

## Cái gì & Tại sao?
Tìm kiếm và thay thế văn bản trong bash có nghĩa là thay thế từ hoặc mẫu trong một chuỗi hoặc tệp bằng thứ gì đó khác. Đây là một nhiệm vụ hàng ngày cho việc làm sạch dữ liệu, sửa chữa mã, hoặc tự động hóa các chỉnh sửa.

## Cách làm:
Dưới đây là cách bạn sử dụng sức mạnh của tìm kiếm và thay thế trong bash:

1. Đổi văn bản trong một chuỗi sử dụng `sed`:
```Bash
echo "Hello world" | sed 's/world/vũ trụ/'
# Kết quả: Hello vũ trụ
```

2. Thay thế văn bản trong một tệp, lưu các thay đổi:
```Bash
sed -i 's/old_text/new_text/g' file.txt
```

3. Sử dụng biến trong tìm kiếm và thay thế của bạn:
```Bash
old="apple"
new="chuối"
sed "s/$old/$new/g" <<< "I like apple pies"
# Kết quả: I like chuối pies
```

Nhớ rằng, `g` ở cuối có nghĩa là "toàn cầu", vì vậy bạn thay đổi mỗi lần khớp trong dòng, không chỉ lần đầu tiên.

## Sâu hơn nữa

Chúng ta đã có công cụ xử lý văn bản trên các hệ thống giống Unix từ rất lâu. `sed`, viết tắt của Stream Editor, là một công cụ như vậy, và nó đã tồn tại từ những năm 1970. Nó không chỉ dùng để thay thế đơn giản; `sed` còn có thể cắt và chia văn bản theo các mẫu phức tạp nữa.

Có lựa chọn khác không? Chắc chắn rồi. `awk` phức tạp hơn một chút và có thể làm việc thần kỳ với các cột và hàng. Đối với các sửa đổi nhanh chóng, `grep` có thể giúp bạn tìm thứ gì đó, nhưng nó sẽ không thay thế - nó giống như người canh gác hơn.

Bên trong, `sed` sử dụng biểu thức chính quy, giống như các ký tự đại diện trên steroid. Chúng có thể khớp hầu hết mọi mẫu bạn có thể nghĩ đến. Điều này làm cho `sed` vô cùng mạnh mẽ, nhưng cũng hơi khó để thành thạo.

## Xem Thêm
- `man sed` cho hướng dẫn sử dụng `sed`
- [Giới thiệu về `sed`](https://www.gnu.org/software/sed/manual/sed.html)
- [Biểu thức chính quy cho người mới bắt đầu](https://www.regular-expressions.info/tutorial.html)
- Nghệ thuật Dòng Lệnh cho thêm mẹo bash (https://github.com/jlevy/the-art-of-command-line)
