---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:05:47.966017-07:00
description: "L\xE0m th\u1EBF n\xE0o: D\u01B0\u1EDBi \u0111\xE2y l\xE0 c\xE1ch m\u1EDF\
  \ c\xE1c t\u1EC7p v\u0103n b\u1EA3n v\u1EDBi Fish Shell."
lastmod: '2024-03-13T22:44:37.234352-06:00'
model: gpt-4-0125-preview
summary: "D\u01B0\u1EDBi \u0111\xE2y l\xE0 c\xE1ch m\u1EDF c\xE1c t\u1EC7p v\u0103\
  n b\u1EA3n v\u1EDBi Fish Shell."
title: "\u0110\u1ECDc m\u1ED9t t\u1EC7p v\u0103n b\u1EA3n"
weight: 22
---

## Làm thế nào:
Dưới đây là cách mở các tệp văn bản với Fish Shell:

```Fish Shell
# Đọc tệp dòng qua dòng
while read -la line
    echo $line
end < file.txt
```

```Fish Shell
# Xuất nội dung của một tệp trực tiếp
cat file.txt
```

Mẫu đầu ra (từ `cat`):

```plaintext
Xin chào, Fish!
Chỉ đang bơi qua các tệp.
```

## Sâu hơn nữa
Ngày xưa, kể cả trước khi Fish Shell ra mắt vào khoảng năm 2005, việc đọc tệp là một nhu cầu thiết yếu. Các shell Unix luôn có các công cụ cho việc này. Vì sao lại chọn Fish? Nó thân thiện, hiện đại và hợp lý với các cài đặt mặc định cho kịch bản, tạo nên một lựa chọn dễ chịu so với các shell cũ hơn.

Vòng lặp `while read` thực sự hữu ích cho việc chỉnh sửa từng dòng. Đừng quên rằng `read` có các cờ như `-la` dùng để tạo biến danh sách từ dòng - rất tốt cho các giá trị được phân tách bằng dấu phẩy.

Ngược lại, `cat` thì trực tiếp và đơn giản. Nó kết hợp và hiển thị nội dung tệp. Nó đã tồn tại trong Unix từ (chà, để xác định thì là từ năm 1971).

Về mặt hiệu suất, đọc trực tiếp thường nhanh hơn và ổn đối với các tệp nhỏ hơn. Nhưng khi bạn có một tệp văn bản cỡ Moby Dick, cân nhắc xử lý từng dòng hoặc các công cụ như `sed`, `awk`, hoặc thậm chí `grep` nếu bạn đang tìm kiếm các dòng cụ thể.

## Xem thêm
- [Tài liệu chính thức của Fish](https://fishshell.com/docs/current/index.html) để sâu vào tất cả mọi thứ về Fish Shell.
- Một [chuỗi thảo luận trên Unix StackExchange](https://unix.stackexchange.com/questions/tagged/fish) cho sự hỗ trợ rộng lớn hơn từ cộng đồng và cái nhìn sâu hơn.
- Một bài hướng dẫn về [sử dụng awk trong kịch bản shell](https://www.gnu.org/software/gawk/manual/gawk.html) có thể sẽ hữu ích nếu có các nhiệm vụ xử lý văn bản phức tạp hơn xuất hiện.
