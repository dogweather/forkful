---
title:                "Sử dụng vỏ tương tác (REPL)"
date:                  2024-01-28T22:10:15.227701-07:00
model:                 gpt-4-0125-preview
simple_title:         "Sử dụng vỏ tương tác (REPL)"

category:             "PHP"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/php/using-an-interactive-shell-repl.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cái gì & Tại sao?
Một shell tương tác, hay REPL (Read-Eval-Print Loop - Vòng lặp Đọc-Đánh giá-In), cho phép bạn viết và chạy mã PHP ngay lập tức. Nó lý tưởng cho việc thử nghiệm, gỡ lỗi, hoặc học hỏi, vì bạn có thể thử nghiệm các đoạn mã mà không cần tới việc tạo một script đầy đủ.

## Làm thế nào:
Khởi chạy REPL PHP bằng cách chạy `php -a` trong terminal của bạn. Dưới đây là một ví dụ về cách nó hoạt động:

```php
php > echo "Hello, World!";
Hello, World!
php > $arr = [1, 2, 3];
php > print_r($arr);
Mảng
(
    [0] => 1
    [1] => 2
    [2] => 3
)
```

Bạn cũng có thể định nghĩa hàm:

```php
php > function sum($a, $b) { return $a + $b; }
php > echo sum(5, 10);
15
```

## Sâu hơn nữa
REPL đã xuất hiện dưới một số hình thức kể từ những ngày đầu của LISP vào những năm 1960. Shell tương tác của PHP kém phát triển so với những ngôn ngữ như Python hay JavaScript. Nó không duy trì trạng thái giữa các phiên và thiếu các tính năng như tự động hoàn thành. Để có một REPL PHP đầy đủ tính năng hơn, hãy xem xét các lựa chọn thay thế như `psysh` hay `boris`. Những shell của bên thứ ba này cung cấp công cụ kiểm tra tốt hơn, hoàn thành tab và thậm chí là một trình gỡ lỗi.

Bên dưới cùng, REPL của PHP hoạt động bằng cách biên dịch và thực thi từng dòng mã khi nó được nhập. Hạn chế của phương pháp này trở nên rõ ràng với những vấn đề như khai báo lại lớp, điều này không thể thực hiện trong cùng một phiên. Nó tốt cho các bài kiểm tra đơn giản nhưng có thể trở nên rườm rà với các nhiệm vụ phức tạp.

## Xem thêm
- [Hướng dẫn PHP - Shell tương tác](https://www.php.net/manual/en/features.commandline.interactive.php)
- [PsySH: Một bảng điều khiển phát triển runtime, gỡ lỗi tương tác và REPL cho PHP](https://psysh.org/)
- [Boris: Một REPL nhỏ cho PHP](https://github.com/borisrepl/boris)
