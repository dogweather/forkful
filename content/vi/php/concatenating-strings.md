---
title:                "Nối chuỗi ký tự"
date:                  2024-01-28T21:58:19.680601-07:00
model:                 gpt-4-0125-preview
simple_title:         "Nối chuỗi ký tự"

category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/php/concatenating-strings.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Gì & Tại Sao?

Nối chuỗi đơn giản chỉ là việc ghép các từ lại với nhau. Hãy tưởng tượng nó giống như việc tạo ra một đoàn tàu từ các từ chứ không phải các toa tàu. Các lập trình viên thực hiện việc này để kết hợp văn bản, như tên với lời chào, hoặc để xây dựng các thông điệp và dữ liệu cần phải linh hoạt.

## Cách thực hiện:

Trong PHP, nối chuỗi tất cả đều quanh điểm nối (`.`). Lấy hai chuỗi, đặt một dấu chấm ở giữa chúng, và voila! Chúng giờ đây đã trở thành một.

```PHP
$greeting = 'Hello, ';
$name = 'Alice!';
$message = $greeting . $name;
echo $message;
// Đầu ra: Hello, Alice!
```

Dễ dàng, phải không? Cần thêm một khoảng trắng? Chỉ cần bao gồm nó trong một chuỗi và nối:

```PHP
$firstWord = 'Hello';
$space = ' ';
$secondWord = 'World!';
$sentence = $firstWord . $space . $secondWord;
echo $sentence;
// Đầu ra: Hello World!
```

Và đối với những người chuyên nghiệp PHP, chúng ta có thể nối chúng lại với nhau hoặc sử dụng cách viết tắt (`.= `):

```PHP
$message = 'This';
$message .= ' is';
$message .= ' a';
$message .= ' sentence.';
echo $message;
// Đầu ra: This is a sentence.
```

## Sâu hơn nữa

Ngày xưa, những người dùng PHP phải sử dụng dấu chấm để ghép các chuỗi lại với nhau. Nó giống như băng dính cho các từ. Việc nối chuỗi là thiết yếu bởi vì dữ liệu không phải lúc nào cũng được truyền đi đúng định dạng cần thiết.

Đối với các phương án thay thế, có vài cái. Các hàm `sprintf()` và `printf()` cho phép tạo chuỗi theo định dạng. Hãy tưởng tượng bạn đang tạo một kịch bản phim với các vị trí trống, và những hàm này điền vào tên của các diễn viên.

```PHP
$format = 'Có %d con khỉ trong %s';
echo sprintf($format, 5, 'cây');
// Đầu ra: Có 5 con khỉ trong cây
```

Nhưng đừng quên người bạn đáng tin cậy, hàm `implode()`. Nó giống như một máy lấy một mảng các chuỗi và một chuỗi keo và kết dính chúng lại với nhau.

```PHP
$array = ['Ngày', 'xưa', 'có', 'một'];
echo implode(' ', $array);
// Đầu ra: Ngày xưa có một
```

Một điều cần xem xét là hiệu suất. Đối với các chuỗi dài hoặc các thao tác nặng, sử dụng `.` có thể chậm hơn so với các phương pháp khác như `implode()` hay thậm chí là đệm đầu ra. Nhưng đối với hầu hết các tác vụ hàng ngày, việc nối chuỗi bằng dấu chấm hoạt động rất hiệu quả.

## Xem thêm

Dành cho những ai khát khao học hỏi thêm:

- Tài liệu chính thức về toán tử chuỗi trong PHP là nơi tuyệt vời để hiểu biết về công cụ của bạn: [Toán tử chuỗi PHP](https://www.php.net/manual/en/language.operators.string.php)
- Để nắm bắt cách định dạng chuỗi nâng cao hơn, hãy xem hàm `sprintf()` và `printf()`: [PHP sprintf()](https://www.php.net/manual/en/function.sprintf.php)
- Nếu bạn đang tìm cách để ghép các phần tử của một mảng lại với nhau, đọc về hàm `implode()`: [PHP implode()](https://www.php.net/manual/en/function.implode.php)
- Dành cho những người quan tâm đến hiệu suất, cuộc thảo luận này về việc nối chuỗi so với các phương pháp khác là khá sáng suốt: [Stack Overflow: Nối Chuỗi Hiệu Quả trong PHP](https://stackoverflow.com/questions/3349753/efficient-string-concatenation-in-php)
