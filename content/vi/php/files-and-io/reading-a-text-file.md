---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:05:21.887635-07:00
description: "\u0110\u1ECDc m\u1ED9t t\u1EC7p v\u0103n b\u1EA3n trong PHP c\xF3 ngh\u0129\
  a l\xE0 k\xE9o n\u1ED9i dung t\u1EEB m\u1ED9t t\u1EC7p v\xE0o script c\u1EE7a b\u1EA1\
  n. L\u1EADp tr\xECnh vi\xEAn l\xE0m \u0111i\u1EC1u n\xE0y \u0111\u1EC3 x\u1EED l\xFD\
  \ vi\u1EC7c l\u01B0u tr\u1EEF d\u1EEF li\u1EC7u, c\u1EA5u h\xECnh,\u2026"
lastmod: '2024-03-13T22:44:36.789614-06:00'
model: gpt-4-0125-preview
summary: "\u0110\u1ECDc m\u1ED9t t\u1EC7p v\u0103n b\u1EA3n trong PHP c\xF3 ngh\u0129\
  a l\xE0 k\xE9o n\u1ED9i dung t\u1EEB m\u1ED9t t\u1EC7p v\xE0o script c\u1EE7a b\u1EA1\
  n. L\u1EADp tr\xECnh vi\xEAn l\xE0m \u0111i\u1EC1u n\xE0y \u0111\u1EC3 x\u1EED l\xFD\
  \ vi\u1EC7c l\u01B0u tr\u1EEF d\u1EEF li\u1EC7u, c\u1EA5u h\xECnh,\u2026"
title: "\u0110\u1ECDc m\u1ED9t t\u1EC7p v\u0103n b\u1EA3n"
weight: 22
---

## Gì & Tại sao?
Đọc một tệp văn bản trong PHP có nghĩa là kéo nội dung từ một tệp vào script của bạn. Lập trình viên làm điều này để xử lý việc lưu trữ dữ liệu, cấu hình, hoặc để xử lý các bộ dữ liệu lớn mà không làm rối code của họ.

## Làm thế nào:
### Sử dụng `file_get_contents`:
```PHP
$content = file_get_contents("example.txt");
echo $content;
```
Mẫu Đầu ra:
```
Hello, World!
This is content from the text file.
```

### Sử dụng `fopen` và `fgets`:
```PHP
$handle = fopen("example.txt", "r");
if ($handle) {
    while (($line = fgets($handle)) !== false) {
        echo $line;
    }
    fclose($handle);
}
```
Mẫu Đầu ra:
```
Hello, World!
This is content from the text file.
```

### Viết vào một tệp với `file_put_contents`:
```PHP
$newContent = "Adding new text.";
file_put_contents("example.txt", $newContent);
```

## Đào sâu
Đọc tệp văn bản cũng như lập trình vậy. Trước khi có cơ sở dữ liệu, các tệp cấu hình và dữ liệu người dùng thường tồn tại trong các tệp văn bản đơn giản. Các phương thức thay thế như tệp XML và JSON được cấu trúc, dễ dàng phân tích hơn và thích hợp cho dữ liệu phức tạp.

Trong PHP, `file_get_contents` và `file()` là nhanh chóng khi đọc; cái đầu tiên lấy tất cả trong một chuỗi, và cái sau trong một mảng. `fopen` kết hợp với `fgets` hoặc `fread` cho bạn nhiều kiểm soát hơn, đặc biệt là đối với các tệp lớn, khi bạn đọc nó từng dòng hoặc theo từng khối.

Một số nét tinh tế: `fopen` đòi hỏi quyền thích hợp, nếu không sẽ thất bại; xử lý lỗi của nó là một phương pháp hay nhất. Khi sử dụng `file_put_contents`, hãy lưu ý rằng nó ghi đè lên tệp theo mặc định; sử dụng cờ `FILE_APPEND` để thêm nội dung thay vì vậy.

## Xem thêm
- Hướng dẫn PHP về `file_get_contents`: https://www.php.net/manual/en/function.file-get-contents.php
- Hướng dẫn PHP về `fopen`: https://www.php.net/manual/en/function.fopen.php
- Hướng dẫn PHP về `fgets`: https://www.php.net/manual/en/function.fgets.php
- Hướng dẫn PHP về `file_put_contents`: https://www.php.net/manual/en/function.file-put-contents.php
- Hướng dẫn về xử lý tệp PHP: https://www.w3schools.com/php/php_file.asp
