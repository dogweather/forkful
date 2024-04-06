---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:05:21.887635-07:00
description: "L\xE0m th\u1EBF n\xE0o: M\u1EABu \u0110\u1EA7u ra."
lastmod: '2024-04-05T21:53:38.169747-06:00'
model: gpt-4-0125-preview
summary: ''
title: "\u0110\u1ECDc m\u1ED9t t\u1EC7p v\u0103n b\u1EA3n"
weight: 22
---

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
