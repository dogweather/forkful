---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:07:20.661159-07:00
description: "C\xE1ch l\xE0m: D\u01B0\u1EDBi \u0111\xE2y l\xE0 c\xE1ch b\u1EA1n s\u1EED\
  \ d\u1EE5ng s\u1EE9c m\u1EA1nh c\u1EE7a t\xECm ki\u1EBFm v\xE0 thay th\u1EBF trong\
  \ bash: 1. \u0110\u1ED5i v\u0103n b\u1EA3n trong m\u1ED9t chu\u1ED7i s\u1EED d\u1EE5\
  ng `sed`."
lastmod: '2024-03-13T22:44:36.857302-06:00'
model: gpt-4-0125-preview
summary: "D\u01B0\u1EDBi \u0111\xE2y l\xE0 c\xE1ch b\u1EA1n s\u1EED d\u1EE5ng s\u1EE9\
  c m\u1EA1nh c\u1EE7a t\xECm ki\u1EBFm v\xE0 thay th\u1EBF trong bash."
title: "T\xECm ki\u1EBFm v\xE0 thay th\u1EBF v\u0103n b\u1EA3n"
weight: 10
---

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
