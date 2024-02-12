---
title:                "Xử lý lỗi"
aliases:
- vi/php/handling-errors.md
date:                  2024-01-28T22:02:26.991365-07:00
model:                 gpt-4-0125-preview
simple_title:         "Xử lý lỗi"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/php/handling-errors.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cái gì & Tại sao?
Xử lý lỗi trong PHP là về quản lý và phản ứng với các điều kiện làm gián đoạn dòng chảy bình thường của chương trình, như tệp tin bị thiếu hoặc dữ liệu nhập không đúng. Các lập trình viên xử lý lỗi để ngăn chặn sự cố và mang lại trải nghiệm mượt mà hơn cho người dùng.

## Cách thực hiện:
Trong PHP, bạn có thể quản lý lỗi bằng cách sử dụng các khối `try-catch`, và bạn có thể tùy chỉnh quy trình với các trình xử lý lỗi tùy chỉnh và ngoại lệ.

```php
// Ví dụ cơ bản về try-catch
try {
  // Thực hiện một cái gì đó có rủi ro
  $file = fopen("nonexistentfile.txt", "r");
} catch (Exception $e) {
  // Xử lý lỗi
  echo "Lỗi: " . $e->getMessage();
}

// Thiết lập trình xử lý lỗi tùy chỉnh
set_error_handler(function($severity, $message, $file, $line) {
  throw new ErrorException($message, 0, $severity, $file, $line);
});

// Sử dụng ngoại lệ
class MyException extends Exception {}

try {
  // Thực hiện một cái gì đó và ném một ngoại lệ tùy chỉnh
  throw new MyException("Lỗi tùy chỉnh!");
} catch (MyException $e) {
  // Xử lý ngoại lệ tùy chỉnh
  echo $e->getMessage();
}

// Đầu ra mẫu:
// Lỗi: fopen(nonexistentfile.txt): không mở được luồng: Không có tệp hoặc thư mục như vậy
// Lỗi tùy chỉnh!
```

## Sâu hơn
Ngày xưa, lỗi PHP thường là về cảnh báo và thông báo mà không dừng thực thi kịch bản. Khi ngôn ngữ phát triển, nó đã áp dụng cách xử lý lỗi hướng đối tượng mạnh mẽ hơn thông qua lớp Exception được giới thiệu trong PHP 5. Sau đó, PHP 7 ra mắt với các lớp Error cuối cùng đã phân biệt giữa lỗi và ngoại lệ.

Trước các khối `try-catch`, PHP đã sử dụng `set_error_handler()` để xử lý lỗi. `try-catch` sạch sẽ, hiện đại hơn. Nhưng trình xử lý lỗi tùy chỉnh vẫn có một vị trí, đặc biệt là đối với mã nguồn cũ hoặc khi bạn cần nắm bắt những gì thường là lỗi không phải ngoại lệ.

Giao diện `Throwable` trong PHP 7+ có nghĩa là dù là Lỗi hay Ngoại lệ, bạn có thể bắt cả hai. Điều này rất tiện lợi vì bây giờ bạn không bỏ lỡ các lỗi runtime quan trọng, which were khó để theo dõi trước đây.

Các phương pháp thay thế ngoài cơ chế tích hợp sẵn của PHP bao gồm các thư viện và frameworks đi kèm với hệ thống xử lý lỗi của riêng mình, cung cấp nhiều tính năng hơn như ghi lỗi vào tệp hoặc hiển thị trang lỗi thân thiện với người dùng.

## Xem thêm
- Tài liệu chính thức của PHP về Ngoại lệ: https://www.php.net/manual/en/language.exceptions.php
- PHP The Right Way về báo cáo lỗi: https://phptherightway.com/#error_reporting
- Sổ tay PHP về Xử lý Lỗi: https://www.php.net/manual/en/book.errorfunc.php
