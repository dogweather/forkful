---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:05:47.966017-07:00
description: "\u0110\u1ECDc m\u1ED9t t\u1EC7p v\u0103n b\u1EA3n l\xE0 vi\u1EC7c l\u1EA5\
  y d\u1EEF li\u1EC7u b\xEAn trong t\u1EC7p \u0111\u1EC3 x\u1EED l\xFD. L\u1EADp tr\xEC\
  nh vi\xEAn l\xE0m \u0111i\u1EC1u n\xE0y \u0111\u1EC3 r\xFAt tr\xEDch th\xF4ng tin,\
  \ c\u1EA5u h\xECnh \u1EE9ng d\u1EE5ng, ph\xE2n t\xEDch log, ho\u1EB7c\u2026"
lastmod: '2024-03-13T22:44:37.234352-06:00'
model: gpt-4-0125-preview
summary: "\u0110\u1ECDc m\u1ED9t t\u1EC7p v\u0103n b\u1EA3n l\xE0 vi\u1EC7c l\u1EA5\
  y d\u1EEF li\u1EC7u b\xEAn trong t\u1EC7p \u0111\u1EC3 x\u1EED l\xFD. L\u1EADp tr\xEC\
  nh vi\xEAn l\xE0m \u0111i\u1EC1u n\xE0y \u0111\u1EC3 r\xFAt tr\xEDch th\xF4ng tin,\
  \ c\u1EA5u h\xECnh \u1EE9ng d\u1EE5ng, ph\xE2n t\xEDch log, ho\u1EB7c\u2026"
title: "\u0110\u1ECDc m\u1ED9t t\u1EC7p v\u0103n b\u1EA3n"
---

{{< edit_this_page >}}

## Cái gì & Tại sao?
Đọc một tệp văn bản là việc lấy dữ liệu bên trong tệp để xử lý. Lập trình viên làm điều này để rút trích thông tin, cấu hình ứng dụng, phân tích log, hoặc chỉ đơn giản là đưa dữ liệu vào một kịch bản.

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
