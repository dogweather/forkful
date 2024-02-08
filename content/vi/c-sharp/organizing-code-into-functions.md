---
title:                "Sắp xếp mã thành các hàm"
aliases:
- vi/c-sharp/organizing-code-into-functions.md
date:                  2024-01-28T22:03:04.883079-07:00
model:                 gpt-4-0125-preview
simple_title:         "Sắp xếp mã thành các hàm"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/c-sharp/organizing-code-into-functions.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cái gì & Tại sao?
Chia code thành các hàm giống như việc sắp xếp các viên LEGO vào trong các hộp — làm cho việc tìm kiếm và sử dụng chúng trở nên dễ dàng hơn. Chúng ta làm điều này để tránh lặp lại, đơn giản hóa việc hiểu và làm cho việc bảo trì ít đau đầu hơn.

## Làm thế nào:
Hãy tưởng tượng bạn có đoạn code in lời chào nhiều lần. Không có hàm, nó sẽ là một mớ hỗn độn. Có hàm, mọi thứ sẽ gọn gàng.

```C#
// Không có hàm - lặp lại
Console.WriteLine("Hello, Amy!");
Console.WriteLine("Hello, Bob!");
Console.WriteLine("Hello, Charlie!");

// Có hàm - sạch sẽ hơn
void Greet(string name) {
    Console.WriteLine($"Hello, {name}!");
}

Greet("Amy");
Greet("Bob");
Greet("Charlie");
```

Kết quả giống nhau, nhưng phiên bản thứ hai sạch sẽ hơn nhiều.

## Sâu hơn
Ngày xưa, trong ngôn ngữ lập trình hợp ngữ, bạn sẽ nhảy đến các vị trí code khác nhau với GOTO — loạn và khó theo dõi. Hàm là một bước tiến lớn, giống như các ngăn kéo được tổ chức trong hộp đồ nghề. Có các phương án thay thế không? Chắc chắn rồi. Bạn có phương thức, là hàm trong ngữ cảnh của lớp. Rồi đến lambda và hàm nội tuyến cho các nhiệm vụ nhanh chóng, một lần.

Về việc triển khai - những hàm nhỏ, tập trung là vàng. Chúng dễ kiểm tra và gỡ lỗi hơn. Những hàm lớn với nhiều trách nhiệm có thể trở nên quái vật, kiếm được danh hiệu đáng ngờ là "mã spaghetti". Giữ mỗi hàm một công việc; bạn sẽ cảm ơn mình sau này.

## Xem thêm
Để biết thêm về hàm và các phương pháp hay nhất, hãy xem:

- Clean Code của Robert C. Martin: Nguyên tắc để giữ cho hàm của bạn gọn gàng.
- Refactoring của Martin Fowler: Cách cải thiện mã hiện có.
- Hướng dẫn về Phương pháp của Microsoft C#: https://docs.microsoft.com/en-us/dotnet/csharp/programming-guide/classes-and-structs/methods
